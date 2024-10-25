import scala.util.Random

object SistemaDeDicas {
  // Função para fornecer uma dica revelando uma letra que ainda não foi revelada
  def fornecerDica(palavra: String, letrasCorretas: Set[Char]): Option[Char] = {
    val letrasFaltantes = palavra.toLowerCase.filter(letra => !letrasCorretas.contains(letra) && letra.isLetter)
    
    if (letrasFaltantes.nonEmpty) {
      Some(letrasFaltantes.charAt(Random.nextInt(letrasFaltantes.length)))
    } else {
      None // Caso todas as letras já tenham sido reveladas
    }
  }

  // Função para verificar se o jogador pode pedir uma dica (precisa ter pelo menos 10 pontos)
  def podeDarDica(jogadorAtual: String, pontosPorJogador: scala.collection.mutable.Map[String, Int]): Boolean = {
    pontosPorJogador.getOrElse(jogadorAtual, 0) >= 10
  }
}

