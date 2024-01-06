import Control.Exception (evaluate)
import Control.Monad
import Data.List (find, nub, nubBy)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.IO

-- Структура данных для работы
data Work = Work
  { uniqueNumber :: Int,
    device :: String,
    service :: String,
    time :: String,
    price :: Int
  }
  deriving (Show)

-- Структура данных для заявки
data Request = Request
  { requestNumber :: Int,
    workNumber :: Int,
    status :: String,
    creationDateTime :: String,
    updateDateTime :: String
  }
  deriving (Show)

-- Функция для чтения данных из файла и преобразования их в список структур Work
readWorksFromFile :: FilePath -> IO [Work]
readWorksFromFile fileName = do
  contents <- readFile fileName
  let lines' = lines contents
  let worksData = map parseWork lines'
  return worksData

-- Функция для чтения данных из файла и преобразования их в список структур Request
readRequestsFromFile :: FilePath -> IO [Request]
readRequestsFromFile fileName = do
  contents <- readFile fileName
  let lines' = lines contents
  let requestsData = map parseRequest lines'
  evaluate (length requestsData)
  return requestsData

-- Преобразовать строку в структуру Work
parseWork :: String -> Work
parseWork line =
  case split ';' line of
    [num, dev, serv, t, prc] ->
      Work (read num) dev serv t (read prc)
    _ ->
      error "Неправильный формат данных"

-- Преобразовать строку в структуру Request
parseRequest :: String -> Request
parseRequest line =
  case split ';' line of
    [num, service, status, createTime, updateTime] ->
      Request (read num) (read service) status createTime updateTime
    _ ->
      error "Неправильный формат данных"

-- Функция для разделения строки по разделителю
split :: Eq a => a -> [a] -> [[a]]
split delimiter input = case break (== delimiter) input of
  (x, []) -> [x]
  (x, rest) -> x : split delimiter (tail rest)

-- Функция для поиска заявки по номеру
findRequestByNumber :: Int -> [Request] -> Maybe Request
findRequestByNumber targetNumber =
  find (\request -> requestNumber request == targetNumber)

-- Проверка, находится ли дата в пределах последних 6 месяцев
isWithinLast6Months :: UTCTime -> Request -> Bool
isWithinLast6Months currentTime request =
  let creationTime = parseTimeOrError True defaultTimeLocale "%d.%m.%Y %H:%M" (creationDateTime request)
      updateTime = parseTimeOrError True defaultTimeLocale "%d.%m.%Y %H:%M" (updateDateTime request)
      sixMonthsAgo = addUTCTime (fromIntegral $ -6 * 30 * 24 * 60 * 60) currentTime
   in creationTime >= sixMonthsAgo || updateTime >= sixMonthsAgo

-- Функция для печати 1 заявки
printRequestFields :: Request -> IO ()
printRequestFields request = do
  putStr $ "Заявка: " ++ show (requestNumber request) ++ "; "

  putStr $ "Статус: " ++ status request ++ ";\t"
  putStr $ "Дата создания: " ++ creationDateTime request ++ ";  "
  putStr $ "Дата обновления: " ++ updateDateTime request ++ ";  "

  workInfo <- getWorkInfoByNumber (workNumber request)
  putStrLn $ "Услуга: " ++ service workInfo ++ " для " ++ device workInfo

-- Функция для печати всех заявок
printRequests :: [Request] -> IO ()
printRequests requests = do
  currentTime <- getCurrentTime
  putStrLn "\nЗаявки за последние 6 месяцев:"
  let filteredRequests = filter (isWithinLast6Months currentTime) requests
  mapM_ printRequestFields filteredRequests

-- Функция для отслеживания заявок на ремонт
trackRequests :: IO ()
trackRequests = do
  requests <- readRequestsFromFile "requests.txt"

  printRequests requests

  putStrLn "\nДля возврата в главное меню нажмите Enter"
  void getLine

-- Функция для получения информации о работе по номеру
getWorkInfoByNumber :: Int -> IO Work
getWorkInfoByNumber workNumber = withFile "jobs.txt" ReadMode $ \handle -> do
  contents <- hGetContents handle
  let workLine = lines contents !! (workNumber - 1)
  let workInfo = parseWork workLine
  seq workInfo (return workInfo)

-- Функция для печати 1 заявки с полной информацией о работе
printRequestInfoDetailed :: Request -> IO ()
printRequestInfoDetailed request = do
  putStrLn $ "\nЗаявка: " ++ show (requestNumber request)

  workInfo <- getWorkInfoByNumber (workNumber request)
  putStrLn $ "Устройство: " ++ device workInfo
  putStrLn $ "Услуга: " ++ service workInfo
  putStrLn $ "Время выполнения: " ++ time workInfo
  putStrLn $ "Стоимость: " ++ show (price workInfo)

  putStrLn $ "Статус: " ++ status request
  putStrLn $ "Дата создания: " ++ creationDateTime request
  putStr $ "Дата обновления: " ++ updateDateTime request

-- Функция для отслеживания информации по конкретной заявке
trackRequest :: IO ()
trackRequest = do
  putStr "\nВведите номер заявки: "
  hFlush stdout
  input <- getLine
  let targetNumber = read input :: Int

  requests <- readRequestsFromFile "requests.txt"

  case findRequestByNumber targetNumber requests of
    Just foundRequest -> printRequestInfoDetailed foundRequest
    Nothing -> putStrLn $ "\nЗаявка с номером " ++ show targetNumber ++ " не найдена."

  putStrLn "\n\nДля возврата в главное меню нажмите Enter"
  void getLine

-- Функция для поиска уникальных устройств в списке работ
uniqueDevices :: [Work] -> [String]
uniqueDevices works = nub $ map device works

-- Функция для вывода названий устройств с номерами
printDeviceNamesWithNumbers :: [String] -> Int -> IO ()
printDeviceNamesWithNumbers [] _ = return ()
printDeviceNamesWithNumbers (name : names) index = do
  putStrLn $ show index ++ ": " ++ name
  printDeviceNamesWithNumbers names (index + 1)

-- Функция для вывода списка устройств
printDeviceList :: [String] -> IO ()
printDeviceList deviceNames = do
  putStrLn "\nВыберите устройство из списка:"
  printDeviceNamesWithNumbers deviceNames 1
  putStrLn "Для добавления устройства введите 0"

-- Функция для считывания номера устройства от пользователя
getUserInput :: IO Int
getUserInput = do
  putStr "Выберите устройство: "
  hFlush stdout
  input <- getLine
  return (read input :: Int)

-- Установка кодировки UTF-8
setUtf8Encoding :: IO ()
setUtf8Encoding = do
  setLocaleEncoding utf8

-- Функция для добавления записи в файл jobs.txt
addJobRecord :: Int -> String -> IO ()
addJobRecord uniqueNumber deviceName = do
  setUtf8Encoding
  putStrLn "Введите данные для новой записи"
  putStr "Услуга: "
  service <- getLine
  putStr "Время выполнения: "
  time <- getLine
  putStr "Стоимость: "
  price <- readLn :: IO Int

  let newRecord = "\n" ++ show uniqueNumber ++ ";" ++ deviceName ++ ";" ++ service ++ ";" ++ time ++ ";" ++ show price

  withFile "jobs.txt" AppendMode $ \handle -> do
    hPutStr handle newRecord

-- Функция для обработки выбора пользователя
handleUserChoice :: [String] -> IO String
handleUserChoice deviceNames = do
  choice <- getUserInput
  if choice == 0
    then do
      putStr "\nВведите название нового устройства: "
      hFlush stdout
      device <- getLine
      lastJobNumber <- getLastUniqueNumber "jobs.txt"
      addJobRecord (lastJobNumber + 1) device
      return device
    else
      if choice > 0 && choice <= length deviceNames
        then return $ deviceNames !! (choice - 1)
        else do
          putStrLn "Некорректный выбор. Пожалуйста, выберите корректный номер устройства."
          handleUserChoice deviceNames

-- Функция для считывания номера выбранной работы от пользователя
getDeviceWorkInput :: IO Int
getDeviceWorkInput = do
  putStr "Выберите работу (введите номер): "
  hFlush stdout
  input <- getLine
  return (read input :: Int)

-- Функция для выбора работы
chooseDeviceWork :: [Work] -> IO Work
chooseDeviceWork works = do
  choice <- getDeviceWorkInput
  if choice >= 1 && choice <= length works
    then return (works !! (choice - 1))
    else do
      putStrLn "Некорректный выбор. Пожалуйста, выберите номер работы из списка."
      chooseDeviceWork works

-- Функция для вывода списка уникальных работ, связанных с устройством
printDeviceWorks :: [Work] -> String -> IO ()
printDeviceWorks works selectedDevice = do
  let uniqueWorks = filter (\w -> device w == selectedDevice) works
  printDeviceWorksHelper uniqueWorks 1

-- Вспомогательная функция для вывода списка работ с номерами
printDeviceWorksHelper :: [Work] -> Int -> IO ()
printDeviceWorksHelper [] _ = return ()
printDeviceWorksHelper (w : ws) index = do
  putStrLn $ show index ++ ": " ++ service w
  printDeviceWorksHelper ws (index + 1)

-- Функция для фильтрации работ по выбранному устройству и работе
findJobUniqueNumber :: String -> Work -> [Work] -> [Work]
findJobUniqueNumber selectedDevice chosenWork works =
  filter (\w -> device w == selectedDevice && service w == service chosenWork) works

-- Функция для добавления записи в конец файла requests.txt
addRequestToFile :: Int -> Int -> IO ()
addRequestToFile uniqueNumberValue workNumberValue = do
  currentTime <- getCurrentTime
  let formattedTime = formatTime defaultTimeLocale "%d.%m.%Y %H:%M" $ utcToLocalTime utc currentTime
  let record = "\n" ++ show (uniqueNumberValue + 1) ++ ";" ++ show workNumberValue ++ ";Создана;" ++ formattedTime ++ ";" ++ formattedTime
  withFile "requests.txt" AppendMode $ \handle -> hPutStr handle record

-- Функция для получения уникального номера из последней строки файла
getLastUniqueNumber :: FilePath -> IO Int
getLastUniqueNumber fileName = withFile fileName ReadMode $ \handle -> do
  contents <- hGetContents handle
  lastLine <- evaluate $ last (lines contents)
  return $ read $ takeWhile (/= ';') lastLine

-- Функция для создания новой заявки
createNewRequest :: IO ()
createNewRequest = do
  works <- readWorksFromFile "jobs.txt"
  let deviceNames = uniqueDevices works
  printDeviceList deviceNames
  selectedDevice <- handleUserChoice deviceNames

  putStrLn $ "\nСоздание заявки на ремонт устройства " ++ selectedDevice

  putStrLn "Список работ для выбранного устройства:"
  works <- readWorksFromFile "jobs.txt"
  printDeviceWorks works selectedDevice
  chosenDeviceWork <- chooseDeviceWork $ filter (\w -> device w == selectedDevice) works
  putStrLn $ "\nВыбранная работа для устройства " ++ selectedDevice ++ ": " ++ service chosenDeviceWork
  let work = findJobUniqueNumber selectedDevice chosenDeviceWork works

  lastRequestNumber <- getLastUniqueNumber "requests.txt"

  addRequestToFile lastRequestNumber (uniqueNumber (head work))
  putStrLn "Запись добавлена в файл requests.txt."

  putStrLn "\nДля возврата в главное меню нажмите Enter"
  void getLine

-- Функция для обновления статуса заявки
updateRequestStatus :: IO ()
updateRequestStatus = do
  putStr "\nВведите номер заявки: "
  requestNumberStr <- getLine
  let targetRequestNumber = read requestNumberStr :: Int
  let fileName = "requests.txt"

  requests <- readRequestsFromFile fileName
  let targetRequest = findRequestByNumber targetRequestNumber requests

  case targetRequest of
    Just request -> do
      putStrLn "\nВыберите действие"
      putStrLn "1) Начать исполнение заявки"
      putStrLn "2) Завершить исполнение заявки"
      putStr "Ваш выбор: "
      actionChoice <- getLine
      currentTime <- getCurrentTime
      let formattedTime = formatTime defaultTimeLocale "%d.%m.%Y %H:%M" $ utcToLocalTime utc currentTime
      let updatedRequest = case actionChoice of
            "1" -> request {status = "В работе", updateDateTime = formattedTime}
            "2" -> request {status = "Завершена", updateDateTime = formattedTime}
            _ -> request

      let updatedRequests = map (\r -> if requestNumber r == requestNumber updatedRequest then updatedRequest else r) requests
      writeRequestsToFile fileName updatedRequests
      putStrLn "\nСтатус заявки успешно обновлен."
      putStrLn "\nДля возврата в главное меню нажмите Enter"
      void getLine
    Nothing -> do
      putStrLn "\nЗаявка не найдена."
      putStrLn "\nДля возврата в главное меню нажмите Enter"
      void getLine

-- Функция для записи заявок в файл
writeRequestsToFile :: FilePath -> [Request] -> IO ()
writeRequestsToFile fileName requests = withFile fileName WriteMode $ \handle -> do
  hPutStr handle $ init $ unlines $ map formatRequest requests

-- Функция для форматирования заявки в строку
formatRequest :: Request -> String
formatRequest request =
  show (requestNumber request)
    ++ ";"
    ++ show (workNumber request)
    ++ ";"
    ++ status request
    ++ ";"
    ++ creationDateTime request
    ++ ";"
    ++ updateDateTime request

-- Главное меню
actionsMenu :: IO ()
actionsMenu = do
  -- putStr "\ESC[2J"
  putStrLn "Меню:"
  putStrLn "1) Отследить заявки на ремонт"
  putStrLn "2) Отследить заявку"
  putStrLn "3) Создать заявку"
  putStrLn "4) Обновить статус заявки"
  putStrLn "5) Выход"
  putStr "Выберите действие: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> trackRequests >> actionsMenu
    "2" -> trackRequest >> actionsMenu
    "3" -> createNewRequest >> actionsMenu
    "4" -> updateRequestStatus >> actionsMenu
    "5" -> putStrLn "Выход"
    _ -> putStrLn "\nНекорректный выбор\n" >> actionsMenu

main :: IO ()
main = actionsMenu