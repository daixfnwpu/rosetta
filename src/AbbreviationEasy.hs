module AbbreviationEasy where
import Data.List (find,isPrefixOf,inits,transpose,intercalate)
import qualified Data.Set  as S
import Data.Char( toUpper,isUpper)
import Data.Random.Distribution.Categorical (normalizeCategoricalPs)
import Data.Maybe (fromMaybe)

commandTable= unwords [
   "Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy",
   "COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find",
   "NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput",
   "Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO",
   "MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT",
   "READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT",
   "RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up"]

isAbbreviationOf abbreviation command = 
    minimumPrefix `isPrefixOf` normalizedAbbrevation
    && normalizedAbbrevation `isPrefixOf` normalizedCommand
    where
        normalizedAbbrevation = map toUpper abbreviation
        normalizedCommand = map toUpper command
        minimumPrefix = takeWhile isUpper command

expandAbbreviation commandTable abbreviation = do
    command <- find (isAbbreviationOf abbreviation) (words commandTable)
    return $ map toUpper command

runMain = do
    input <- getLine
    let abbreviations = words input
    let commands = map (fromMaybe "*error*" . expandAbbreviation commandTable) abbreviations
    putStrLn $ unwords commands
