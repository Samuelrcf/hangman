object Util {
  // Função para converter a palavra em uma string de hífens
  def palavraParaHifens(palavra: String): String = {
    palavra.map { char =>
      if (char.isWhitespace) ' ' else '-'
    }.mkString
  }

  // Função para exibir a palavra atual com os caracteres acertados
  def exibirPalavraAtual(palavra: String, letrasCorretas: Set[Char]): String = {
    palavra.map { caractere =>
      if (caractere == ' ') {
        ' ' // Mantém o espaço
      } else if (letrasCorretas.contains(caractere.toLower)) {
        caractere // Mostra a letra se foi adivinhada
      } else {
        '-' // Substitui por hífen se não foi adivinhada
      }
    }.mkString
  }
}
