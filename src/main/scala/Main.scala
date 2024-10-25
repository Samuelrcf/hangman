import scala.util.Random

object JogoDaForca {
  @main def init(): Unit = {
    println("Bem-vindo ao Jogo da Forca!")
    println("-------------------------------------------------------------------------------------------------------------------------------------")
    println("Regras:")
    println("1. O jogador tem direito a uma letra por rodada.")
    println("2. Se o jogador acertar uma letra, ele obtém 10 pontos.")
    println("3. Caso o jogador erre a letra ou digite alguma entrada inválida, nada acontece.")
    println("4. O jogador tem direito a uma dica por rodada usando a tecla [1]. Cada dica custa 10 pontos.")
    println("5. Caso o jogador peça uma dica sem ter pontos suficientes, passa a vez.")
    println("6. O jogador tem direito a adivinhar a palavra a qualquer momento utilizando a tecla [0].")
    println("7. Caso o jogador acerte a palavra, ele obtém a metade dos pontos equivalentes da palavra, exceto se estiver restando apenas 1 letra.")
    println("8. No modo multiplayer, caso o jogador erre a palavra, os outros participantes obtêm a metade dos pontos equivalentes da palavra dividida igualmente entre eles.")
    println()

    val arquivo = "/palavras.txt" 

    val mapaDeTemas = LeitorDePalavras.lerArquivo(arquivo)

    println("Temas disponíveis:")
    mapaDeTemas.keys.zipWithIndex.foreach { case (tema, index) =>
      println(s"[${index + 1}] $tema")
    }

    println("\nEscolha um tema pelo número:")
    val numeroEscolhido = scala.io.StdIn.readLine().toInt

    if (numeroEscolhido < 1 || numeroEscolhido > mapaDeTemas.size) {
      println("Escolha inválida. O jogo será encerrado.")
      return
    }

    val temaEscolhido = mapaDeTemas.keys.toList(numeroEscolhido - 1)

    println("Quantos jogadores participarão? (1 ou mais):")
    val numeroJogadores = scala.io.StdIn.readLine().toInt

    val palavraEscolhida = LeitorDePalavras.escolherPalavraAleatoriaComTamanhoMinimo(mapaDeTemas, temaEscolhido, numeroJogadores * 3)

    palavraEscolhida match {
      case Some(palavra) =>
        if (palavra.length < numeroJogadores * 3) {
          println(s"Não há palavras suficientes com pelo menos ${numeroJogadores * 3} caracteres para este tema.")
          return
        }

        val jogadores = scala.collection.mutable.ListBuffer[String]()
        var pontosPorJogador = scala.collection.mutable.Map[String, Int]()

        for (i <- 1 to numeroJogadores) {
          println(s"Nome do jogador $i:")
          val nome = scala.io.StdIn.readLine()
          jogadores += nome
          pontosPorJogador(nome) = 0
        }

        var indiceJogadorAtual = 0
        var letrasCorretas = Set[Char]()
        val pontosTotais = palavra.count(_ != ' ') * 10 // Contar apenas letras, ignorando espaços

        println(s"\nPalavra escolhida: ${Util.palavraParaHifens(palavra)}")

        while (true) {
          val jogadorAtual = jogadores(indiceJogadorAtual)
          println(s"Tema escolhido: $temaEscolhido")
          println(s"Jogador: $jogadorAtual")
          println("Insira um caractere, digite [0] para tentar adivinhar a palavra, ou digite [1] para pedir uma dica (10 pontos):")
          val entrada = scala.io.StdIn.readLine().toLowerCase

          if (entrada == "1") {
            if (SistemaDeDicas.podeDarDica(jogadorAtual, pontosPorJogador)) {
              SistemaDeDicas.fornecerDica(palavra, letrasCorretas) match {
                case Some(letraDica) =>
                  letrasCorretas += letraDica
                  pontosPorJogador(jogadorAtual) -= 10
                  println(s"Dica: a letra '$letraDica' foi revelada. Você perdeu 10 pontos.")
                case None =>
                  println("Não há mais letras para revelar!")
              }
            } else {
              println("Você não tem pontos suficientes para pedir uma dica.")
            }
          } else if (entrada == "0") {
            println(s"Você escolheu tentar adivinhar a palavra. Por favor, digite a palavra completa:")
            val tentativaPalavra = scala.io.StdIn.readLine().toLowerCase

            if (tentativaPalavra == palavra.toLowerCase) {
              // Verificação se resta apenas 1 letra para ser adivinhada
              val letrasFaltando = palavra.count(c => !letrasCorretas.contains(c) && c != ' ') // Ignorar espaços
              val letrasTotais = palavra.count(_ != ' ') // Contar apenas letras, ignorando espaços
              val pontosGanhos = if (letrasFaltando == 1) 10 else letrasTotais * 10 / 2

              println(s"Parabéns, $jogadorAtual! Você adivinhou a palavra!")
              pontosPorJogador(jogadorAtual) += pontosGanhos
              println(s"Você ganhou $pontosGanhos pontos.")

              Ranking.atualizarRanking(pontosPorJogador)
              return
            } else {
              println(s"Palavra errada! A palavra era: $palavra.")

              // Dividir os pontos entre os jogadores restantes
              val pontosPerdidos = pontosTotais / 2
              val jogadoresRestantes = jogadores.filterNot(_ == jogadorAtual)
              val pontosPorJogadorRestante = pontosPerdidos / jogadoresRestantes.size

              jogadoresRestantes.foreach { jogador =>
                pontosPorJogador(jogador) += pontosPorJogadorRestante
                println(s"$jogador ganhou $pontosPorJogadorRestante pontos.")
              }

              Ranking.atualizarRanking(pontosPorJogador)
              return
            }

          } else if (entrada.length == 1) {
            val caractere = entrada.charAt(0)

            if (palavra.toLowerCase.contains(caractere)) {
              letrasCorretas += caractere
              pontosPorJogador(jogadorAtual) += 10
              println("Você acertou um caractere!")
            } else {
              println("Caractere não existe na palavra!")
            }

          } else {
            println("Entrada inválida. Insira apenas um caractere, pressione [0] para tentar adivinhar a palavra ou [1] para pedir uma dica.")
          }

          val palavraAtual = Util.exibirPalavraAtual(palavra, letrasCorretas)
          println(s"Palavra: $palavraAtual")

          if (!palavraAtual.contains('-')) {
            println(s"Parabéns, $jogadorAtual! Você adivinhou a palavra!")

            Ranking.atualizarRanking(pontosPorJogador)
            return
          }

          indiceJogadorAtual = (indiceJogadorAtual + 1) % jogadores.size
        }

      case None =>
        println("Nenhuma palavra encontrada para o tema escolhido.")
    }
  }
}
