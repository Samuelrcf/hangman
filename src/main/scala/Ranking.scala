import scala.io.Source
import java.io.{BufferedWriter, FileWriter}

object Ranking {
  // Função para atualizar o ranking e salvar em um arquivo
  def atualizarRanking(pontosPorJogador: scala.collection.mutable.Map[String, Int]): Unit = {
    val arquivoRanking = "ranking.txt"
    val pontosExistentes = scala.collection.mutable.Map[String, Int]()

    // Ler o arquivo de ranking existente, se houver
    try {
      val source = Source.fromFile(arquivoRanking)
      for (linha <- source.getLines()) {
        val partes = linha.split(":")
        if (partes.length == 2) {
          val jogador = partes(0).trim
          val ponto = partes(1).trim.toInt
          pontosExistentes(jogador) = ponto
        }
      }
      source.close()
    } catch {
      case _: Exception => // Ignorar se o arquivo não existir
    }

    // Atualizar os pontos
    pontosPorJogador.foreach { case (nome, pontos) =>
      val pontosNovos = pontosExistentes.getOrElse(nome, 0) + pontos
      pontosExistentes(nome) = pontosNovos
    }

    // Salvar no arquivo
    val writer = new BufferedWriter(new FileWriter(arquivoRanking))
    for ((jogador, ponto) <- pontosExistentes) {
      writer.write(s"$jogador: $ponto\n")
    }
    writer.close()
  }
}
