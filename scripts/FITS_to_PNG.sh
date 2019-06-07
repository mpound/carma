#!/bin/bash
#Author: Daniel Finch
# searches the "star-[yyyy]-[mm]-[dd]" directories for FITS format photos and converts them into PNG format using ds9
# and indexes them on a webpage in the specified web directory. 

#LOCAL_DIR="/home/obs"
#WEB_DIR="/misc/array/utilities/apache2.0.54/htdocs/pointing/opticalPointing/results"
LOCAL_DIR=`pwd`
WEB_DIR="${LOCAL_DIR}/web"
HELPFILE=`echo -e "USAGE: $0 [-d][-l Local directory][-w Web directory][-r Broken sub-directory] \n\n -d\t Runs with default directories: ./ and ./web. \n\n -l\t Specifies the local directory to find \"stars-yyyy-mm-dd\" sub-directories in. \n\n -w\t Specifies the destination directory for the webpage and converted images.\n\n -r\t Repairs missing images in specified sub-directory. \n\nDESCRIPTION\n\tUses a FITS viewer program called ds9 to convert FITS images into PNG images and indexes the images in a webpage."`
MAX_TABLE_WIDTH=1024


if [ "$#" -eq 0 ];then
	echo "${HELPFILE}";exit 1
fi
# getopts example from http://www.computing.net/answers/unix/help-let-script-accept-arguments/4794.html
# Use getopts to scan for -d/-l:/-w:/-r:
# (':' after option letter indicates arg expected.
# Leading ':' in optstring suppresses default error messages.)
while getopts ":dl:w:r:" opt; do
case $opt in
d ) echo "Running with defaults: $LOCAL_DIR and $WEB_DIR"; break;;
l ) LOCAL_DIR=$OPTARG;
	if [ -d "$LOCAL_DIR" ];then
		echo "Local directory set to $LOCAL_DIR";else	
		echo "$0: ERROR: Local directory: $LOCAL_DIR not found";exit 1
	fi;;
w ) WEB_DIR=$OPTARG;
	if [ -d "$WEB_DIR" ];then
		echo "Web directory set to $WEB_DIR";else
		echo "$0: ERROR: Web directory: $WEB_DIR not found";exit 1
	fi;;
r ) REPAIR=$OPTARG;
	if [ -d "$LOCAL_DIR/$REPAIR" ];then
		echo "Repair sub-directory set to $REPAIR";else
		echo "$0: ERROR: Sub-directory: $REPAIR not found";exit 1
	fi;;
* ) echo  "${HELPFILE}"; exit 1 ;;
esac
done
shift $(($OPTIND -1)) #remove options leave arguments
######################################################################

#Get the full paths, not . or ./web 
BASE_DIR=`pwd`
cd $LOCAL_DIR
LOCAL_DIR=`pwd`
cd $BASE_DIR
cd $WEB_DIR
WEB_DIR=`pwd`
cd $LOCAL_DIR

Date_index="<html><title>Pointing Results</title><h2> Date </h2>Pick a date to see the results for that date."
for STAR_DIR in stars-20*
do	
	#rebuild the date index page.
	Date=`echo $STAR_DIR | awk -F'-' 'BEGIN{OFS="-"}{print $3,$4,$2;}'`
	Formatted_Date="<a href=\"./${STAR_DIR}/index.html\">${Date}</a>"
	Date_index=`echo "${Date_index} <br> ${Formatted_Date}"`
	if [ $REPAIR ]; then
		STAR_DIR=`echo $REPAIR`;fi
	#if the photos for a date are already processed, skip to the next date.
	if ([[ -d $WEB_DIR/$STAR_DIR ]] && [ ! $REPAIR ])
	then 
		echo "${STAR_DIR} complete" 
	else
		#process the photos for each day.
		mkdir -p $WEB_DIR/$STAR_DIR
		Photo_index="<html><title>Pointing Results for ${Date}</title><h2>Images for ${Date} sorted by target and antenna</h2><table>"
		target=""
		picture_OPs=""
		tableWidth=0		# don't stretch the page too far.
		frame=0
		cd $LOCAL_DIR/$STAR_DIR
		for fitsNAME in *.fits
		do
		NAME=${fitsNAME%.fits}
		nextTarget=`echo $NAME | awk -F'-' '{ print $1 }'`
		antenna=`echo $NAME | awk -F'-' '{ print $2 }'`
		if [[ "$target" != "$nextTarget" ]]
		then
			target=$nextTarget
			tableWidth=0
			Photo_index=`echo -n "${Photo_index} </table><br><br><table border=\"1\"><th>${target}</th><tr>"`
		fi 
		let "tableWidth = $tableWidth + 100"
		if(($tableWidth > $MAX_TABLE_WIDTH))
		then
			tableWidth=0
			Photo_index=`echo "${Photo_index} </table><table border=\"1\">"`
		fi 
		Photo_index=`echo "${Photo_index} <td><a href=\"./${NAME}.png\"><img src=\"./${NAME}.png\"WIDTH=100 HEIGHT=150></a><br>${antenna}</td>"`
		if ([ ! $REPAIR ] || [[ ! -e $WEB_DIR/$STAR_DIR/$NAME.png ]]); then
#			echo converting $fitsNAME into $NAME.png using ds9
			fitsDIM=`identify $fitsNAME | awk '{ print $3 }'`
			fitsWIDTH=${fitsDIM%x*}  
			fitsHEIGHT=${fitsDIM#*x}
			
			picture_OPs=`echo -n "${picture_OPs} ${LOCAL_DIR}/${STAR_DIR}/${fitsNAME} -frame hide ${frame} -height ${fitsHEIGHT} -width ${fitsWIDTH} -saveimage png ${WEB_DIR}/${STAR_DIR}/${NAME}.png"`		
			((frame = frame + 1))
			if [ $frame -ge 500 ]; then
				cd $LOCAL_DIR
				ds9 -view colorbar no ${picture_OPs} -exit
				cd $LOCAL_DIR/$STAR_DIR
				picture_OPs=""
				frame=0
			fi
		fi
		done
		Photo_index=`echo "${Photo_index} </tr></table></html>"`	
		cd $LOCAL_DIR
		ds9 -view colorbar no ${picture_OPs} -exit
		echo $Photo_index > $WEB_DIR/$STAR_DIR/index.html
		if [ $REPAIR ]; then
			echo "$REPAIR repaired"
			cd $BASE_DIR
			exit ;fi
		echo "${STAR_DIR} complete"
	fi
done
Date_index=`echo -e "${Date_index} \\n </html>"`
echo "${Date_index}" > $WEB_DIR/index.html
cd $BASE_DIR
