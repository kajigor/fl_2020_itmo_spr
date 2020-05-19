package parse.info

import java.nio.file.Path

interface ParseReportMaker: BaseParseFormatter {
    fun makeReport(filePath: Path)
}
