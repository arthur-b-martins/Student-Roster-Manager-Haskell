{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Integrantes: Matheus Henrique de Andrade Pires (12411BCC061)
               Arthur Borges Martins (12411BCC063) 
               
Instruções: Para compilar o programa basta digitar no terminal do Ubuntu ghc Main 
Para executar o programa, digite no terminal do Ubuntu ./Main             
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}
module Main (main) where

-- BIBLIOTECAS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

import System.Directory (doesFileExist)
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering), writeFile)
import Data.Char(toLower)
import Data.Maybe (Maybe(Just))
import Data.List (maximum, minimum, sort,intercalate)
import Control.DeepSeq (deepseq)
import Text.Printf (printf)

-- MAIN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main :: IO ()
main = menuLoop "" ""

-- MENU PRINCIPAL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Função para imprimir o menu e pegar a opção do usuário
menuLoop :: String -> String -> IO ()
menuLoop conteudo caminho = do 
  hSetBuffering stdout NoBuffering
  menuPrincipal conteudo caminho
  putStr("Digite uma opção: ")
  e <- getLine
  escolhaMenu e conteudo caminho

-- Menu principal com informações das opções
menuPrincipal :: String -> String -> IO ()
menuPrincipal conteudo caminho = do
  menuGeral "Menu Principal" 
  putStrLn("1 - Ler turma de estudantes do arquivo")
  putStrLn("2 - Imprimir turma de estudantes")
  putStrLn("3 - Imprimir estatísticas da turma")
  putStrLn("4 - Cadastrar novo estudante")
  putStrLn("5 - Editar informações de um estudante")
  putStrLn("6 - Reler turma de estudantes do arquivo")
  putStrLn("7 - Salvar e Sair\n")

-- Função para avaliar a opção selecionada pelo usuário
escolhaMenu :: String -> String -> String -> IO ()
escolhaMenu e conteudo caminho = case e of
  "1" -> lerTurma conteudo caminho
  "2" -> imprimirTurma conteudo caminho 
  "3" -> calcularEstatisticas conteudo caminho
  "4" -> cadastrarNovo conteudo caminho
  "5" -> editarInfo conteudo caminho
  "6" -> relerArquivo conteudo caminho
  "7" -> salvarSair conteudo caminho 
  _   -> putStrLn "Opção inválida" >> menuLoop "" ""

-- 1 - LER TURMA DE ESTUDANTES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Função para chamar o menu de leitura de turma (Figura 2)
lerTurma :: String -> String -> IO ()
lerTurma conteudo caminho = do
  menuGeral "Ler Turma de Estudantes"
  putStrLn("1 - Ler turma existente")
  putStrLn("2 - Criar Nova Turma")
  putStrLn("3 - Voltar ao menu principal\n")
  putStr("Digite uma opção: ")
  e <- getLine
  escolhaLerTurma e conteudo caminho

-- Função para processar a opção escolhida
escolhaLerTurma :: String -> String -> String -> IO ()
escolhaLerTurma e conteudo caminho = case e of
  "1" -> lerDadosTurma conteudo caminho
  "2" -> criarNovaTurma
  "3" -> menuLoop conteudo caminho
  _   -> putStrLn "Opção inválida" >> menuLoop "" ""

-- 1 - Ler turma existente
lerDadosTurma :: String -> String -> IO ()
lerDadosTurma conteudo caminho = do  
  putStr "Digite o nome do arquivo com a turma: "
  caminho <- getLine 
  exists <- doesFileExist caminho
  if exists
    then lerArquivoTurma conteudo caminho
    else do
      putStrLn ("\nArquivo " ++ caminho ++ " nao existe!\n")
      lerTurma conteudo caminho

-- Função para ler o arquivo caso exista
lerArquivoTurma :: String -> String -> IO ()
lerArquivoTurma conteudo caminho = do
  conteudo <- readFile caminho 
  putStrLn ("\nArquivo " ++ caminho ++ " lido com sucesso!")
  lerTurma conteudo caminho

-- 2 - Criar nova turma
criarNovaTurma :: IO ()
criarNovaTurma = do
  putStr "Digite o nome do arquivo para a nova turma: "
  novaTurma <- getLine
  exists <- doesFileExist novaTurma

  if exists 
    then sobreescrever novaTurma
    else criarTurma novaTurma

-- Cria nova turma caso ela não exista
criarTurma :: String -> IO ()
criarTurma novaTurma = do
  writeFile novaTurma ""
  putStrLn("\nArquivo " ++ novaTurma ++ " criado com sucesso.\n")
  menuLoop "" novaTurma

-- Dá a opção de sobreescrever outro arquivo, caso um arquivo com o mesmo nome já exista
sobreescrever :: String -> IO ()
sobreescrever novaTurma = do
  putStrLn("\nArquivo " ++ novaTurma ++ " ja existe. Deseja sobrescreve-lo? (S/N)")
  op <- getLine
  let opLower = toLower (head op)

  case opLower of
    's' -> writeFile novaTurma "" >> putStrLn ("\nTurma " ++ novaTurma ++ " sobrescrita!\n") >> lerTurma "" novaTurma
    'n' -> lerTurma "" ""
    _   -> putStrLn "Opcao Invalida!" >> sobreescrever novaTurma

-- 2 - IMPRIMIR TURMA DE ESTUDANTES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Imprimir turma caso haja uma carregada
imprimirTurma :: String -> String -> IO ()
imprimirTurma conteudo caminho = do 
  if null caminho
    then do
      putStrLn "Você deve primeiro carregar uma turma!"
      menuLoop conteudo caminho
    else do
      cabecalho
      let listaAlunos =  lerAlunos (conteudo) 
      mapM_ mostrarAluno (listaAlunos)
      putStrLn "\n"
      voltarMenu conteudo caminho 

-- Mostra um aluno
mostrarAluno :: Aluno -> IO ()
mostrarAluno aluno = do
  putStrLn (nomeF ++ " " ++ m ++ " " ++ n1F ++ n2F ++ n3F ++ t1F ++ t2F ++ fF ++ nFF ++ s)

  where 
    nomeF = nome aluno ++ " " ++ replicate (49 - length(nome aluno)) '-'
    m = matricula aluno
    n1 = show (nota1 aluno)
    n1F = n1 ++ replicate (5 - length n1) ' ' 
    n2 = show (nota2 aluno)
    n2F = n2 ++ replicate (5 - length n2) ' '
    n3 = show (nota3 aluno)
    n3F = n3 ++ replicate (5 - length n3) ' '
    t1 = show (trab1 aluno)
    t1F = t1 ++ replicate (5 - length t1) ' '
    t2 = show (trab2 aluno)
    t2F = t2 ++ replicate (5 - length t2) ' '
    f = show (faltas aluno)
    fF = f ++ replicate (8 - length f) ' '
    nF = show (notaFinal aluno)
    nFF = nF ++ replicate (7 - length nF) ' '
    s = show (situacao aluno)

-- Imprime cabeçalho
cabecalho :: IO ()
cabecalho = do 
  putStrLn "______________________________________________________________________________________________________________"
  putStrLn "                                             Turma de Estudantes "
  putStrLn "______________________________________________________________________________________________________________"
  putStrLn "Nome                                               Matrícula   N1   N2   N3   T1   T2   Faltas  Final  Situacao\n\n"

-- 3 - IMPRIMIR ESTATÍSTICAS DA TURMA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Função para calcular/chamar funções auxíliares para extrair informações dos dados
calcularEstatisticas :: String -> String -> IO ()
calcularEstatisticas conteudo caminho = do
  if null caminho 
    then do
      putStrLn "\nNão há turma carregada!\n"
      menuLoop conteudo caminho
    else if null conteudo 
      then do
        putStrLn "\nNão há alunos na turma!\n"
        menuLoop conteudo caminho
      else do 
        let notas1 = lerNotas conteudo "n1"
        let notas2 = lerNotas conteudo "n2"
        let notas3 = lerNotas conteudo "n3"
        let trab1 = lerNotas conteudo "t1"
        let trab2 = lerNotas conteudo "t2"
        let notasFinais = lerNotas conteudo "nf"
        let numeroAprovados = countSituacao conteudo "aprovado"
        let numeroRecuperacao = countSituacao conteudo "recuperacao"
        let numeroReprovados = countSituacao conteudo "reprovado"

        imprimirEstatisticas notas1 notas2 notas3 trab1 trab2 notasFinais numeroAprovados numeroRecuperacao numeroReprovados

        menuLoop conteudo caminho

-- Função para imprimir as informações
imprimirEstatisticas :: [Float] -> [Float] -> [Float] -> [Float] -> [Float] -> [Float] -> Int -> Int -> Int -> IO ()
imprimirEstatisticas notas1 notas2 notas3 trab1 trab2 notasFinais numeroAprovados numeroRecuperacao numeroReprovados = do
  putStrLn "-------------------------------------------------------------------------"
  putStrLn "                               Estatistica                               "
  putStrLn "-------------------------------------------------------------------------"
  putStrLn ("                        " ++ unwords ["N1      ", "N2      ", "N3      ", "T1      ", "T2     ", "Final"])

  putStrLn "-------------------------------------------------------------------------"
  putStrLn ("Maiores notas da turma  " ++ printf "%.1f     " (maximum notas1)      ++ printf "%.1f     " (maximum notas2)      ++ printf "%.1f     " (maximum notas3)      ++ printf "%.1f     " (maximum trab1)      ++ printf "%.1f     " (maximum trab2)      ++ printf "%.1f" (maximum notasFinais))
  putStrLn ("Menores notas da turma   " ++ printf "%.1f     " (minimum notas1)      ++ printf "%.1f     " (minimum notas2)      ++ printf "%.1f     " (minimum notas3)      ++ printf "%.1f     " (minimum trab1)      ++ printf "%.1f     " (minimum trab2)      ++ printf "%.1f" (minimum notasFinais))
  putStrLn ("Notas médias da turma   " ++ printf "%.1f     " (mediaNotas notas1)   ++ printf "%.1f     " (mediaNotas notas2)   ++ printf "%.1f     " (mediaNotas notas3)   ++ printf "%.1f     " (mediaNotas trab1)   ++ printf "%.1f     " (mediaNotas trab2)   ++ printf "%.1f" (mediaNotas notasFinais))
  putStrLn ("Desvios da média         " ++ printf "%.1f      " (desvioPadrao notas1) ++ printf "%.1f      " (desvioPadrao notas2) ++ printf "%.1f     " (desvioPadrao notas3) ++ printf "%.1f     " (desvioPadrao trab1) ++ printf "%.1f     " (desvioPadrao trab2) ++ printf "%.1f" (desvioPadrao notasFinais))
  putStrLn ("Notas medianas da turma " ++ printf "%.1f     " (notaMediana notas1)  ++ printf "%.1f     " (notaMediana notas2)  ++ printf "%.1f     " (notaMediana notas3)  ++ printf "%.1f     " (notaMediana trab1)  ++ printf "%.1f     " (notaMediana trab2)  ++ printf "%.1f" (notaMediana notasFinais))
  putStrLn "-------------------------------------------------------------------------"

  putStrLn ("\nNumero de estudantes aprovados:             " ++ show numeroAprovados ++ " (" ++ show (calculatePorcentage numeroAprovados notas1) ++ "%)")
  putStrLn ("Numero de estudantes em recuperacao:        " ++ show numeroRecuperacao ++ " (" ++ show (calculatePorcentage numeroRecuperacao notas1) ++ "%)")
  putStrLn ("Numero de estudantes reprovados por falta:  " ++ show numeroReprovados ++ " (" ++ show (calculatePorcentage numeroReprovados notas1) ++ "%)\n")

  putStrLn "Histograma de notas finais em grupos de 10 pontos:"
  putStrLn (" 0 -  10   " ++ show (countUntil notasFinais 10) ++ " " ++ (replicate (countUntil notasFinais 10) '*'))
  putStrLn ("11 -  20   " ++ show ((countUntil notasFinais 20) - (countUntil notasFinais 10)) ++ " " ++ (replicate ((countUntil notasFinais 20) - (countUntil notasFinais 10)) '*'))
  putStrLn ("21 -  30   " ++ show ((countUntil notasFinais 30) - (countUntil notasFinais 20)) ++ " " ++ (replicate ((countUntil notasFinais 30) - (countUntil notasFinais 20)) '*' ))
  putStrLn ("31 -  40   " ++ show ((countUntil notasFinais 40) - (countUntil notasFinais 30)) ++ " " ++ (replicate ((countUntil notasFinais 40) - (countUntil notasFinais 30)) '*' ))
  putStrLn ("41 -  50   " ++ show ((countUntil notasFinais 50) - (countUntil notasFinais 40)) ++ " " ++ (replicate ((countUntil notasFinais 50) - (countUntil notasFinais 40)) '*' ))
  putStrLn ("51 -  60   " ++ show ((countUntil notasFinais 60) - (countUntil notasFinais 50)) ++ " " ++ (replicate ((countUntil notasFinais 60) - (countUntil notasFinais 50)) '*' ))
  putStrLn ("61 -  70   " ++ show ((countUntil notasFinais 70) - (countUntil notasFinais 60)) ++ " " ++ (replicate ((countUntil notasFinais 70) - (countUntil notasFinais 60)) '*' ))
  putStrLn ("71 -  80   " ++ show ((countUntil notasFinais 80) - (countUntil notasFinais 70)) ++ " " ++ (replicate ((countUntil notasFinais 80) - (countUntil notasFinais 70)) '*' ))
  putStrLn ("81 -  90   " ++ show ((countUntil notasFinais 90) - (countUntil notasFinais 80)) ++ " " ++ (replicate ((countUntil notasFinais 90) - (countUntil notasFinais 80)) '*' ))
  putStrLn ("91 - 100   " ++ show ((countUntil notasFinais 100) - (countUntil notasFinais 90)) ++ " " ++ (replicate ((countUntil notasFinais 100) - (countUntil notasFinais 90)) '*' ++ "\n" ))

-- Função que retorna a contagem de elementos de uma lista que são menores que um limite
countUntil :: [Float] -> Float -> Int
countUntil xs limit = auxCount xs limit 0
  where
    auxCount [] limit count = count
    auxCount (x:xs) limit count | x < limit || x == limit = auxCount xs limit (count + 1)
                                  | otherwise = auxCount xs limit count

-- Conta o número de alunos aprovados, em recuperação ou reprovados no conteúdo do arquivo mediante os seus respectivo parâmetros
countSituacao :: String -> String -> Int
countSituacao conteudo situacaoEsperada = auxCount (lines conteudo) situacaoEsperada 0
  where
    auxCount [] situacaoEsperada n = n 
    auxCount (_ : _ : _ : _ : _ : _ : _ : _ : _ : situacao:xs) situacaoEsperada n
      | situacaoEsperada == "aprovado" = let situacaoMin = toLower (head situacao)
                                      in case situacaoMin of
                                        'a' -> auxCount xs situacaoEsperada (n + 1)
                                        _ -> auxCount xs situacaoEsperada n
      | situacaoEsperada == "recuperacao" = let situacaoMin = toLower (head situacao)
                                      in case situacaoMin of
                                        'r' -> auxCount xs situacaoEsperada (n + 1)
                                        _ -> auxCount xs situacaoEsperada n
      | situacaoEsperada == "reprovado" = let situacaoMin = toLower (head situacao)
                                      in case situacaoMin of
                                        'f' -> auxCount xs situacaoEsperada (n + 1)
                                        _ -> auxCount xs situacaoEsperada n
      | otherwise = error "Erro na formatacao do arquivo!\n"

-- Calcula o desvio padrão global e NÃO o da amostra                                                                              
desvioPadrao :: [Float] -> Float 
desvioPadrao [] = 0
desvioPadrao xs = sqrt (mediaNotas (map (\x -> (x - mediaNotas xs) ^ 2) xs))

-- Acha a mediana de uma lista que contem as notas de um determinado exame
notaMediana ::  [Float] -> Float
notaMediana [] = error "Sem notas por aqui!"
notaMediana xs | (length xs) `mod` 2 == 0 = (((sort xs) !! mid) + ((sort xs) !! (mid - 1))) / 2
               | otherwise = (sort xs) !! mid
               where
                mid = length xs `div` 2

-- Calcula a média das notas de uma determinada lista
mediaNotas :: [Float] -> Float
mediaNotas [] = 0
mediaNotas xs = soma xs / fromIntegral (length xs)
  where
    soma [] = 0
    soma (x:xs) = x + (soma xs)

-- Extrai do conteúdo as notas referentes ao parâmetro
lerNotas :: String -> String -> [Float]
lerNotas conteudo op = auxLerNotas op (lines conteudo)
  where 
    auxLerNotas op [] = []
    auxLerNotas op (_ : _ : not1 : not2 : not3 : tr1 : tr2 : falt : notF : _ : xs) = 
      case op of
        "n1" -> read not1 : auxLerNotas op xs
        "n2" -> read not2 : auxLerNotas op xs 
        "n3" -> read not3 : auxLerNotas op xs
        "t1" -> read tr1 : auxLerNotas op xs
        "t2" -> read tr2 : auxLerNotas op xs
        "nf" -> read notF : auxLerNotas op xs

-- 4 - CADASTRAR NOVO ESTUDANTE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Verifica se há turma carregada e chama aux
cadastrarNovo :: String -> String -> IO ()
cadastrarNovo conteudo caminho = do 
  if null caminho 
    then do
      putStrLn "\nNão há turma carregada!\n\n"
      menuLoop conteudo caminho 
    else do 
      menuGeral "Cadastrar Novo Estudante"
      verificaNome conteudo caminho

-- Solicita um nome e verifica se é válido, caso seja, chama aux
verificaNome :: String -> String -> IO ()
verificaNome conteudo caminho = do 
  putStr "Digite o nome do aluno: "
  n <- getLine
  if length n > 50
    then do 
      putStrLn "O nome deve ter no máximo 50 caracteres!"
      verificaNome conteudo caminho 
    else do
      let nomeFormatado = removeEspaco n
      if null conteudo
        then do 
          let conteudoNovo =  nomeFormatado
          verificaMatricula conteudoNovo caminho
        else do
          let conteudoNovo = conteudo ++ "\n" ++ nomeFormatado
          verificaMatricula conteudoNovo caminho

-- Solicita matrícula e verifica se é válida, caso seja, ele chama aux
verificaMatricula :: String -> String -> IO ()
verificaMatricula conteudo caminho = do
  putStr "Digite a matrícula do aluno: "
  m <- getLine
  if length m /= 11
    then do 
      putStrLn "A matrícula deve ter 11 caracteres!"
      verificaMatricula conteudo caminho 
    else do
      let conteudoNovo = conteudo ++ "\n" ++ m
      notas conteudoNovo caminho 

-- Solicita notas e faltas
notas :: String -> String -> IO ()
notas conteudo caminho = do 
  putStrLn "Digite as três notas de provas: "
  n1 <- getLine
  n2 <- getLine
  n3 <- getLine
  putStrLn "Digite as duas notas de trabalho: "
  t1 <- getLine
  t2 <- getLine 
  putStr "Digite o numero de faltas: "
  faltas <- getLine

  let nf = calculaNotaFinal n1 n2 n3 t1 t2 
  let s = situacaoFinal faltas nf 

  let str = "Situação final " ++ situacaoString s
  let nfc = "Nota final calculada: " ++ show nf 
  putStrLn nfc 
  putStrLn str
  
  let nfStr = show nf
  let conteudoNovo = conteudo ++ "\n" ++ n1 ++ "\n" ++ n2 ++ "\n" ++ n3 ++ "\n" ++ t1 ++ "\n" ++ t2 ++ "\n" ++ faltas ++ "\n" ++ nfStr ++ "\n" ++ [s]
  menuLoop conteudoNovo caminho 

-- Calcula a nota final
calculaNotaFinal :: String -> String -> String -> String -> String ->  Int
calculaNotaFinal n1 n2 n3 t1 t2 = (read n1) + (read n2) + (read n3) + (read t1) + (read t2)

-- Verifica situação final
situacaoFinal :: String -> Int -> Char 
situacaoFinal faltas nf
  | (read faltas) > 18 = 'F'
  | nf > 59 = 'A'
  | otherwise = 'R'

-- Transforma situação em texto
situacaoString :: Char -> String 
situacaoString s 
  | s == 'A' = "Aprovado"
  | s == 'F' = "Reprovado por faltas"
  | s == 'R' = "Recuperação"

-- 5 - EDITAR INFORMAÇÕES DE UM ESTUDANTE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Verifica se há turma e alunos na turma, caso haja, chama aux
editarInfo :: String -> String -> IO ()
editarInfo conteudo caminho = do 
  if null caminho
    then do
      putStrLn "Não há turma carregada!"
      menuLoop conteudo caminho 
    else do     
      if null conteudo 
        then do 
          putStrLn "\nNão há alunos na turma!\n"
          menuLoop conteudo caminho 
        else do 
          menuGeral "Editar Informações De Um Estudante" 
          putStrLn "1 - Selecionar por número de matrícula"
          putStrLn "2 - Selecionar por nome"
          putStrLn "3 - Voltar ao menu principal\n"
          putStr "Digite uma opção: "
          e <- getLine
          matriculaOuNome e conteudo caminho

-- Verifica opção selecionada no menu
matriculaOuNome :: String -> String -> String -> IO ()
matriculaOuNome e conteudo caminho = case e of 
  "1" -> editarMatricula conteudo caminho
  "2" -> editarNome conteudo caminho 
  "3" -> menuLoop conteudo caminho 
  _   -> putStrLn "Opção inválida" >> editarInfo conteudo caminho 

-- Recebe matrícula e chama aux
editarMatricula :: String -> String ->  IO ()
editarMatricula conteudo caminho = do
  putStr "Digite o número de matrícula: "
  m <- getLine
  acharMatricula conteudo caminho m 

-- Acha aluno por matrícula e chama aux
acharMatricula :: String -> String -> String -> IO ()
acharMatricula conteudo caminho m = do
  if null aluno
    then do 
      putStrLn "\nNúmero de matrícula inválido!" >> editarInfo conteudo caminho 
    else do 
      menuEdicao conteudo caminho (head aluno) alunos
    where
      listaAlunos =  lerAlunos conteudo
      aluno = matriculaAuxEncontrar listaAlunos m 
      alunos = matriculaAuxRemove listaAlunos m []  

-- Encontra e retorna o aluno encontrado
matriculaAuxEncontrar :: [Aluno] -> String -> [Aluno] 
matriculaAuxEncontrar [] m = []
matriculaAuxEncontrar (x:xs) m
  | m == matricula x = [x]
  | otherwise = matriculaAuxEncontrar xs m 

-- Retira o aluno da lista de alunos para que ele seja alterado
matriculaAuxRemove :: [Aluno] -> String -> [Aluno] -> [Aluno]  
matriculaAuxRemove [] m acc = acc 
matriculaAuxRemove (x:xs) m acc 
  |m == matricula x = matriculaAuxRemove xs m acc
  |otherwise        = matriculaAuxRemove xs m (acc ++ [x])

-- Recebe o nome do aluno 
editarNome :: String -> String -> IO ()
editarNome conteudo caminho = do
  putStr "Digite o nome do aluno: "
  n <- getLine
  acharNome conteudo caminho n 

-- Verifica se o aluno está no lista de alunos e chama o proximo menu
acharNome :: String -> String -> String -> IO ()
acharNome conteudo caminho n = do
  if null aluno
    then do 
      putStrLn "\nAluno não encontrado!" >> editarInfo conteudo caminho 
    else do 
      menuEdicao conteudo caminho (head aluno) alunos
    where
      listaAlunos =  lerAlunos conteudo
      aluno = nomeAuxEncontrar listaAlunos n 
      alunos = nomeAuxRemove listaAlunos n []  

-- Encontra e retorna o aluno 
nomeAuxEncontrar :: [Aluno] -> String -> [Aluno] 
nomeAuxEncontrar [] m = []
nomeAuxEncontrar (x:xs) m
  | m == nome x = [x]
  | otherwise = nomeAuxEncontrar xs m 


-- Remove o aluno da lista de alunos para que ele seja alterado
nomeAuxRemove :: [Aluno] -> String -> [Aluno] -> [Aluno]  
nomeAuxRemove [] m acc = acc 
nomeAuxRemove (x:xs) m acc 
  |m == nome x = nomeAuxRemove xs m acc
  |otherwise   = nomeAuxRemove xs m (acc ++ [x])
 
-- Mostra o menu com as opções para serem editadas 
menuEdicao :: String -> String -> Aluno -> [Aluno] -> IO ()
menuEdicao conteudo caminho aluno alunos = do 
  menuGeral "Editar informações do estudante"
  putStrLn "1 - Alterar nome"
  putStrLn "2 - Alterar matrícula"
  putStrLn "3 - Alterar nota da prova 1"
  putStrLn "4 - Alterar nota da prova 2"
  putStrLn "5 - Alterar nota da prova 3"
  putStrLn "6 - Alterar nota do trabalho 1"
  putStrLn "7 - Alterar nota do trabalho 2"
  putStrLn "8 - Alterar numero de faltas"
  putStrLn "9 - Voltar ao menu anterior"
  putStr "\nDigite sua escolha: "
  e <- getLine 
  escolhaEdicao e conteudo caminho aluno alunos

-- Verifica a escolha e chama a função equivalente 
escolhaEdicao :: String -> String -> String -> Aluno -> [Aluno] -> IO ()
escolhaEdicao e conteudo caminho aluno alunos = case e of 
  "1" -> alterarNome conteudo caminho aluno alunos
  "2" -> alterarMatricula conteudo caminho aluno alunos 
  "3" -> alterarNota1 conteudo caminho aluno alunos
  "4" -> alterarNota2 conteudo caminho aluno alunos
  "5" -> alterarNota3 conteudo caminho aluno alunos
  "6" -> alterarTrabalho1 conteudo caminho aluno alunos
  "7" -> alterarTrabalho2 conteudo caminho aluno alunos
  "8" -> alterarFaltas conteudo caminho aluno alunos
  "9" -> salvarEVoltar conteudo caminho aluno alunos 
  _   -> putStr "Escolha inválida!" >> menuLoop conteudo caminho 

-- Salva o aluno editado na lista de alunos (não no arquivo) e volta ao menu anterior 
salvarEVoltar :: String -> String -> Aluno -> [Aluno] -> IO ()
salvarEVoltar conteudo caminho aluno alunos = do
  let alunosNovo = alunos ++ [aluno]
  let conteudoNovo = alunosParaString alunosNovo
  let conteudoNovoCerto = removeLinha (lines conteudoNovo)
  editarInfo conteudoNovo caminho

-- Remove a primeira linha da string ('\n') para evitar erro na formatação do arquivo 
removeLinha :: [String] -> String 
removeLinha linhas = unlines (tail linhas)

-- Transforma uma lista de alunos para uma string da maneira que vamos salvar no arquivo 
alunosParaString :: [Aluno] -> String
alunosParaString alunos =  intercalate "\n" (map alunoParaStr alunos)
  where 
    alunoParaStr :: Aluno -> String
    alunoParaStr aluno = nome aluno ++ "\n" ++
                         matricula aluno ++ "\n" ++
                         show (nota1 aluno) ++ "\n" ++
                         show (nota2 aluno) ++ "\n" ++
                         show (nota3 aluno) ++ "\n" ++
                         show (trab1 aluno) ++ "\n" ++
                         show (trab2 aluno) ++ "\n" ++
                         show (faltas aluno) ++ "\n" ++
                         show (notaFinal aluno) ++ "\n" ++
                         (situacao aluno : [])


-- Altera o nome do aluno
alterarNome :: String -> String -> Aluno -> [Aluno] -> IO ()
alterarNome conteudo caminho aluno alunos = do 
  putStr "Digite o nome: "
  novoNome <- getLine
  let novoAluno = aluno {
        nome = novoNome
        }
  menuEdicao conteudo caminho novoAluno alunos

-- Altera a matrícula 
alterarMatricula :: String -> String -> Aluno -> [Aluno] -> IO ()
alterarMatricula conteudo caminho aluno alunos = do 
  putStr "Digite a matrícula: "
  novaMatricula <- getLine
  let novoAluno = aluno {
        matricula = novaMatricula
        }
  menuEdicao conteudo caminho novoAluno alunos

-- Altera a primeira nota do aluno 
alterarNota1 :: String -> String -> Aluno -> [Aluno] -> IO ()
alterarNota1 conteudo caminho aluno alunos = do 
  putStr "Digite a nota 1: "
  novaNota1 <- getLine
  let novoAluno = aluno {
        nota1 = read novaNota1,
        notaFinal = notaF (read novaNota1) (nota2 aluno) (nota3 aluno) (trab1 aluno) (trab2 aluno),
        situacao = situacaoF (faltas aluno) (notaFinal novoAluno)
        }
  menuEdicao conteudo caminho novoAluno alunos

-- Altera a segunda nota 
alterarNota2 :: String -> String -> Aluno -> [Aluno] -> IO ()
alterarNota2 conteudo caminho aluno alunos = do 
  putStr "Digite a nota 2: "
  novaNota2 <- getLine
  let novoAluno = aluno {
        nota2 = read novaNota2,
        notaFinal = notaF (nota1 aluno) (read novaNota2) (nota3 aluno) (trab1 aluno) (trab2 aluno),
        situacao = situacaoF (faltas aluno) (notaFinal novoAluno)
        }
  menuEdicao conteudo caminho novoAluno alunos

-- Altera a terceira nota
alterarNota3 :: String -> String -> Aluno -> [Aluno] -> IO ()
alterarNota3 conteudo caminho aluno alunos = do 
  putStr "Digite a nota 3: "
  novaNota3 <- getLine
  let novoAluno = aluno {
        nota3 = read novaNota3,
        notaFinal = notaF (nota1 aluno) (nota2 aluno) (read novaNota3) (trab1 aluno) (trab2 aluno),
        situacao = situacaoF (faltas aluno) (notaFinal novoAluno)
        }
  menuEdicao conteudo caminho novoAluno alunos

-- Altera a nota do primeiro trabalho 
alterarTrabalho1 :: String -> String -> Aluno -> [Aluno] -> IO ()
alterarTrabalho1 conteudo caminho aluno alunos = do 
  putStr "Digite o trabalho 1: "
  novoTrab1 <- getLine
  let novoAluno = aluno {
        trab1 = read novoTrab1,
        notaFinal = notaF (nota1 aluno) (nota2 aluno) (nota3 aluno) (read novoTrab1) (trab2 aluno),
        situacao = situacaoF (faltas aluno) (notaFinal novoAluno)
        }
  menuEdicao conteudo caminho novoAluno alunos

-- Altera a nota do segundo trabalho 
alterarTrabalho2 :: String -> String -> Aluno -> [Aluno] -> IO ()
alterarTrabalho2 conteudo caminho aluno alunos = do 
  putStr "Digite o trabalho 2: "
  novoTrab2 <- getLine
  let novoAluno = aluno {
        trab2 = read novoTrab2,
        notaFinal = notaF (nota1 aluno) (nota2 aluno) (nota3 aluno) (trab1 aluno) (read novoTrab2),
        situacao = situacaoF (faltas aluno) (notaFinal novoAluno)
        }
  menuEdicao conteudo caminho novoAluno alunos

-- Altera o numero de faltas 
alterarFaltas :: String -> String -> Aluno -> [Aluno] -> IO ()
alterarFaltas conteudo caminho aluno alunos = do 
  putStr "Digite o número de faltas: "
  nDeFaltas <- getLine
  let novoAluno = aluno {
        faltas = read nDeFaltas,
        situacao = situacaoF (read nDeFaltas) (notaFinal aluno)
        }
  menuEdicao conteudo caminho novoAluno alunos

-- Retorna a nota final 
notaF :: Float -> Float -> Float -> Float -> Float -> Float 
notaF n1 n2 n3 n4 n5 = n1 + n2 + n3 + n4 + n5

-- Retorna a situação final 
situacaoF ::Int -> Float -> Char 
situacaoF faltas nf
  | faltas > 18 = 'F'
  | nf > 59 = 'A'
  | otherwise = 'R'

-- 6 - RELER TURMA DE ESTUDANTES DO ARQUIVO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Função para reler um arquivo
relerArquivo :: String -> String -> IO ()
relerArquivo conteudo caminho = do
  if caminho /= ""
    then do
      writeFile caminho conteudo
      novoConteudo <- readFile caminho
      putStrLn ("\nArquivo " ++ caminho ++ " lido com sucesso!")
      imprimirTurma novoConteudo caminho

    else do
      putStrLn "Erro! Nenhum arquivo foi carregado!"
      menuLoop conteudo caminho

-- 7 - SALVAR E SAIR ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

salvarSair :: String -> String -> IO ()
salvarSair conteudo caminho = do
  if null caminho
    then do
      putStrLn "Nenhum arquivo para ser salvo."
    else do
      deepseq conteudo (return())
      writeFile caminho conteudo
      putStrLn "\nSalvando arquivo...\n" 
  putStr "\nSaindo do programa...\n\n"

-- FUNÇÕES AUXILIARES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Função para calcular o percentual correspondente a n em relação ao tamanho da lista
calculatePorcentage :: Int -> [Float] -> Float
calculatePorcentage count list = if length list == 0 then 0 else (100 * fromIntegral count) / fromIntegral (length list)

-- Remove espaços em branco no inicio e no final da String 
removeEspaco :: String -> String
removeEspaco s = removeAux (reverse (removeAux (reverse s)))
  where
    removeAux [] = []
    removeAux (x:xs)
      | x == ' '  = removeAux xs 
      | otherwise = x : xs

-- Pergunta ao usuario se ele deseja voltar ao menu principal ou sair 
voltarMenu :: String -> String -> IO ()
voltarMenu conteudo caminho = do
  menuGeral "Voltar"
  putStrLn "1 - Voltar ao menu principal"
  putStrLn "2 - Sair e Salvar\n"
  putStr "Digite uma opção: "
  e <- getLine 
  voltarMenuEscolha conteudo caminho e

-- Chama a função adequada 
voltarMenuEscolha :: String -> String -> String -> IO ()
voltarMenuEscolha conteudo caminho e = case e of
  "1" -> menuLoop conteudo caminho
  "2" -> salvarSair conteudo caminho 
  _   -> putStrLn "Opção inválida" >> voltarMenu conteudo caminho

-- Recebe o conteudo do arquivo e retorna uma lista de alunos
lerAlunos :: String -> [Aluno] 
lerAlunos conteudo = lerAlunosAux (lines conteudo)
  where
    lerAlunosAux [] = []
    lerAlunosAux (x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:xs) = 
      Aluno {
        nome = x1,
        matricula = x2,
        nota1 = read x3,
        nota2 = read x4,
        nota3 = read x5,
        trab1 = read x6,
        trab2 = read x7,
        faltas = read x8,
        notaFinal = read x9,
        situacao = head x10
        } : lerAlunosAux (xs)
    lerAlunosAux _ = error "Dados formatados de maneira incorrreta!"

-- Imprime um menu com a mensagem centralizada 
menuGeral :: String -> IO ()
menuGeral msg = do
  putStrLn (replicate 110 '=')
  putStrLn msgCentralizada
  putStrLn (replicate 110 '=')
  putStrLn ("Opções:\n")
 where
  padding = (110 - (length msg)) `div` 2
  msgCentralizada = replicate padding ' ' ++ msg 

-- TIPO 'ALUNO'  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data Aluno = Aluno {
    nome :: String,          -- Nome do aluno (até 50 caracteres)
    matricula :: String,     -- Matrícula (11 caracteres)
    nota1 :: Float,          -- Primeira nota de prova
    nota2 :: Float,          -- Segunda nota de prova
    nota3 :: Float,          -- Terceira nota de prova
    trab1 :: Float,          -- Primeira nota de trabalho
    trab2 :: Float,          -- Segunda nota de trabalho
    faltas :: Int,           -- Número de faltas
    notaFinal :: Float,      -- Nota final
    situacao :: Char         -- Situação final ('A', 'R' ou 'F')
} deriving (Show, Read)
