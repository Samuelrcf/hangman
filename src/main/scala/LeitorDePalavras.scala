import scala.io.{Source, Codec}
import scala.util.Random

object LeitorDePalavras {
  // Função para ler o arquivo e organizar palavras por temas
  def lerArquivo(arquivo: String): Map[String, List[String]] = {
    val source = Source.fromInputStream(getClass.getResourceAsStream(arquivo))(Codec.UTF8)
    val linhas = source.getLines().toList
    source.close()

    var temaAtual: String = ""
    var mapaDeTemas = Map[String, List[String]]()

    linhas.foreach { linha =>
      if (!linha.trim.isEmpty) {
        if (!linha.head.isWhitespace) {
          temaAtual = linha.trim // Remove qualquer formatação extra
          mapaDeTemas += (temaAtual -> List())
        } else {
          val palavra = linha.trim
          mapaDeTemas += (temaAtual -> (mapaDeTemas(temaAtual) :+ palavra))
        }
      }
    }
    mapaDeTemas
  }

  // Função para escolher uma palavra aleatória com tamanho mínimo
  def escolherPalavraAleatoriaComTamanhoMinimo(mapaDeTemas: Map[String, List[String]], tema: String, tamanhoMinimo: Int): Option[String] = {
    mapaDeTemas.get(tema).flatMap { palavras =>
      val palavrasFiltradas = palavras.filter(_.length >= tamanhoMinimo)
      if (palavrasFiltradas.nonEmpty) Some(palavrasFiltradas(Random.nextInt(palavrasFiltradas.size)))
      else None
    }
  }
}
