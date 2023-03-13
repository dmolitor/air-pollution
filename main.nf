/*
 *  Plot Figure 1 for report
 */
Channel
    .fromPath("$projectDir/src/plot.R")
    .set { ch_plotScript_forPlotFig1 }

process PlotFig1 {
    publishDir "$projectDir/src/", mode: 'move', overwrite: true
    
    input:
    path 'plot.R'

    output:
    path 'mobility-and-pollution.png'
    
    """
    Rscript plot.R mobility-and-pollution.png
    """
}

/*
 *  Render report as html docs
 */
process RenderDocs {
    publishDir "$projectDir/docs/", mode: 'move', overwrite: true

    input:
    path 'mobility-and-pollution.png'

    """
    quarto render $projectDir/src/
    touch $projectDir/docs/.nojekyll
    """
}

/*
 *  Execute workflow
 */
workflow {
    PlotFig1(ch_plotScript_forPlotFig1)
    RenderDocs(PlotFig1.out)
}