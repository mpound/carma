# $Id: standardArrayPlots.py,v 1.6 2012/12/19 16:42:42 obs Exp $
# Standardized plotting functions for various array health parameters
#
# @Author James W. Lamb, Caltech
#
# History
# 20-Oct-2008: JWL      Standardized help info format
# 13-Jun-2008: JWL      Added error handling for non Xterm
# 06-Jun-2008: Original version
#

# Required imports

import pylab as plt
from matplotlib.font_manager import FontProperties
from printFunctions import printError

# Globals common to all functions
maxAntennas = 15

# Functions

def allAntennaMultiPlot(xData, yData, antennaList, xMin = None, xMax = None,
                        yMin = None, yMax = None, \
                        orientation = 'portrait', plotTitle = "", xLabel = "x-axis", \
                        yLabel = "y-axis", infoTextLeft = "", infoTextRight = "",
                        showGrid = True, showPlot = True, axisStyle = 'lin', \
                        lineStyles = ('bo-', 'rx--', 'gv-.'), legends = None, \
                        plotFileName = "", makePdf = False, makePng = False) :

    """ Makes an array of plot panels of some selected quantity for all antennas in the array.

        This function takes a list of x-values (common for all antennas, or one per antenna)
        and a list of lists of y-values (one or more lists per antenna) and makes an array of
        plots, one for each antenna, on a single page. A minimal amount of optional
        formatting is available, and two text fields are provided for additional information
        (such as date/time, LO frequency, etc.)

        Parameters:

            xData
                A list of values to use on abscissa of all plots
                [ x1, x2, ..., xn]

                -or-

                A list of lists, one for each antenna

                [
                    [ x1, x2, ..., xn],     # For antenna 1
                    [ x1, x2, ..., xn],     # For antenna 2
                        .
                        .
                        .
                    [ x1, x2, ..., xn]     # For antenna 15
                ]

            yDate
                A list of lists of values to use for ordinates on plots
                [
                    [y1a, y2a, ..., yna],   # For first plot, antenna 1
                    [y1b, y2b, ..., ynb],   # For second plot, antenna 1
                    [y1a, y2a, ..., yna],   # For first plot, antenna 2
                    [y1b, y2b, ..., ynb],   # For second plot, antenna 2
                    .
                    .
                    .
                    [y1a, y2a, ..., yna],   # For first plot, antenna 15
                    [y1b, y2b, ..., ynb],   # For second plot, antenna 15
                ]

            antennaList
                A list of antennas to plot. NOTE: even though an antenna is not
                present in this list, it *must* have the corresponding yData values
                (though contents are not important)

            xMin, xMax, yMin, yMax    {None, None, None, None}
                Limit values for the x- and y-axes. A value of 'None' will autoscale,
                but *beware* that the axis tick values are not shown for every plot!

            orientation = 'portrait'
                Plot page orientation ('portrait' | 'landscape'}

            plotTitle = ""
                Text to be placed at top of graphs

            xLabel, yLabel = "", ""
                Labels for the x- and y-axes

            infoTextLeft, infoTextRight =  "", ""
                Text plotted at the top-left and top-right of the page. Can be used
                to annotate date, time, etc. May contain line breaks ('\n').

            showGrid = True
                Draw grid on all plots.

            showPlot = True
                Show plot on screen.

            lineStyles = ('bo-', 'rx--', 'gv-.')
                A tuple or list with the line styles to use (see below). If the list has
                fewer members than the number of plots on each graph the list is
                repeated.

            legends = ""
                A list or tuple of legend strings that will be listed on the lower left
                of the page. There should be one per plot on a graph.

            plotFileName = ""
                Name of file to save the plots in. Depending on the values of 'makePdf'
                and 'makePng', it will create files '<plotFileName>.pdf' and/or
                '<plotFileName>.png' containing graphics in the corresponding formats.

        Note:

        The following line styles are supported::

            -     # solid line
            --    # dashed line
            -.    # dash-dot line
            :     # dotted line
            .     # points
            ,     # pixels
            o     # circle symbols
            ^     # triangle up symbols
            v     # triangle down symbols
            <     # triangle left symbols
            >     # triangle right symbols
            s     # square symbols
            +     # plus symbols
            x     # cross symbols
            D     # diamond symbols
            d     # thin diamond symbols
            1     # tripod down symbols
            2     # tripod up symbols
            3     # tripod left symbols
            4     # tripod right symbols
            h     # hexagon symbols
            H     # rotated hexagon symbols
            p     # pentagon symbols
            |     # vertical line symbols
            _     # horizontal line symbols
            steps # use gnuplot style 'steps' # kwarg only

        The following color abbreviations are supported::

            b  # blue
            g  # green
            r  # red
            c  # cyan
            m  # magenta
            y  # yellow
            k  # black
            w  # white
    """
#
    error = False
    plt.ioff()

# Number of line styles provided
    numStyles = len(lineStyles)

# Various page and geometry dimensions in inches
    topMargin = 0.1
    bottomMargin = 0.1
    leftMargin = 0.2
    rightMargin = 0.2

    if orientation == 'landscape' :
        pageWidth  = 11.0
        pageHeight = 8.5
    else :
        pageWidth  = 8.5
        pageHeight = 11.0

# Plot panel geometry
    numPlotCols = 3
    numPlotRows = maxAntennas / numPlotCols

    plotLeftDist = 0.75
    plotRightDist = 0.5
    plotTopDist = 1.0
    plotBotDist = 1.0

    plotHeight = (pageHeight - plotTopDist - plotBotDist) / numPlotRows
    plotWidth  = (pageWidth - plotLeftDist - plotRightDist) / numPlotCols

# Some handy font definitions
    tickFont = {'family' : 'sans-serif',
            'weight' : 'normal',
            'size'   : 8,
           }
    generalFont = {'family' : 'sans-serif',
            'weight' : 'normal',
            'size'   : 11,
           }
    plt.rc('font', **generalFont)  # pass in the font dict as kwargs

    titleFontSize = 14
    labelFontSize = 11
    tickFontSize = 8
    infoFontSize = 8
    legendFontSize = 8

# Start a new figure
    try:
        figure = plt.figure(figsize = (pageWidth, pageHeight))
    except :
        printError("allAntennaMultiPlot: Not an Xterm? Cannot plot")
        plt.rc({'backend' : 'Agg'})
        error = True
        return error

# Title for the plots
    titleOffset = 0.05
    x = (0.5 * (pageWidth + plotLeftDist - plotRightDist)) / pageWidth
    y = 1.0 - (plotTopDist - titleOffset) / pageHeight
    plt.figtext(x, y, plotTitle, fontsize = titleFontSize, \
                va = 'bottom', ha = 'center', variant = 'small-caps')

# Left info box
    left = leftMargin / pageWidth
    top = 1.0 - topMargin / pageHeight
    plt.figtext(left, top, infoTextLeft, fontsize = infoFontSize, va = 'top')

# Right info box
    right = 1.0 - rightMargin / pageWidth
    top = 1.0 - topMargin / pageHeight

    plt.figtext(right, top, infoTextRight, fontsize = infoFontSize, va = 'top', \
                ha = 'right')

# Array of plot panels. Start at top left and work left to right
# The array (list of lists) of y values is assumed to be a multiple of the number
# of antennas, with the values for each antenna adjacent
    plotsPerAntenna = len(yData) / maxAntennas
    bot = (pageHeight - plotTopDist - plotHeight) / pageHeight
    ant = 1
    ny = 0

    for row in range(numPlotRows) :
        left = plotLeftDist / pageWidth
        for col in range(numPlotCols) :
            ax = plt.axes([left, bot, plotWidth / pageWidth, plotHeight / pageHeight])
            if showGrid :
                ax.grid(True, color = 'gray')
            plt.figtext(left + plotWidth / pageWidth - 0.01, bot + 0.01, \
                        "C%d" % ant, fontsize = 10, ha = 'right')
            if isinstance(xData[0], list) :
                xd = xData[ant - 1]
            else :
                xd = xData
            if ant in antennaList :
                for nplt in range(plotsPerAntenna) :
                    if axisStyle == 'logx' :
                        plt.semilogx(xd, yData[ny], lineStyles[nplt % numStyles])
                    elif axisStyle == 'logy' :
                        plt.semilogy(xd, yData[ny], lineStyles[nplt % numStyles])
                    elif axisStyle == 'loglog' :
                        plt.loglog(xd, yData[ny], lineStyles[nplt % numStyles])
                    else :
                        plt.plot(xd, yData[ny], lineStyles[nplt % numStyles])
                    ny += 1
            else :
                plt.figtext(left + 0.5 * plotWidth / pageWidth, \
                            bot + 0.5 * plotHeight / pageHeight, "NOT PRESENT", \
                            va = 'center', ha = 'center', color = 'gray', fontsize = 8)
                ny += plotsPerAntenna

        # Insert legend if required
            if (col == 0) and (row == numPlotRows -1) and legends :
                x = -(plotLeftDist - leftMargin) / plotWidth
                y = -(plotBotDist - bottomMargin) / plotHeight
                plt.legend(legends, loc = (x, y), \
                           prop = FontProperties(size = legendFontSize), labelspacing = 0.0)

        # Set up x-axis
            plt.xlim(xMin, xMax)
            if row < numPlotRows - 1 :
                for tick in ax.xaxis.get_major_ticks() :
                    tick.label1On = False
            else :
                if (col < numPlotCols - 1) :
                    ticks = ax.xaxis.get_major_ticks()
                    ticks[len(ticks) - 1].label1On = False
                plt.xticks(**tickFont)

        # Set up y-axis
            plt.ylim(yMin, yMax)
            if col > 0 :
                for tick in ax.yaxis.get_major_ticks() :
                    tick.label1On = False
            else :
                if (row > 0) :
                    ticks = ax.yaxis.get_major_ticks()
                    ticks[len(ticks) - 1].label1On = False
                plt.yticks(**tickFont)

            if (col == numPlotCols - 1) and (row == numPlotRows - 1) :
                plt.xlabel(xLabel)
            if (col == 0) and (row == 0) :
                plt.ylabel(yLabel)
            left += plotWidth / pageWidth
            ant += 1
        bot -= plotHeight / pageHeight

 # Where plot output is to be directed
    if plotFileName :
        if makePdf :
            try :
                plt.savefig(plotFileName + ".pdf")
            except :
                error = True
                printError("Cannot make PDF file")
        if makePng :
            try :
                plt.savefig(plotFileName + ".png")
            except :
                error = True
                printError("Cannot make PNG file")
    if showPlot :
        plt.ioff()
        plt.show()

    return error

#end allAntennaMultiPlot
