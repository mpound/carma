package carma.observertools.pdbi.utils;

import carma.observertools.*;
import carma.observertools.pdbi.utils.comparators.*;
import carma.observertools.pdbi.config.*;

import javax.swing.*;
import javax.swing.filechooser.FileSystemView;

import java.awt.*;
import java.io.File;
import java.text.*;
import java.util.*;

/**
 * class for displaying the results of various searches
 * @author friedel
 *
 */
@SuppressWarnings("serial")
public class ResultsPane extends JPanel{
	public ResultsPane(Project[] projects, String band, String sort, TogglePane jtp, SortType sortType){
		band_ = band;
		setPreferredSize(new Dimension(Globals.WIDTH,543));
		setMinimumSize(new Dimension(Globals.WIDTH,543));
		setMaximumSize(new Dimension(Globals.WIDTH,543));
		setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(),"Obsblocks Found"));
		Tobsblock_ = new ArrayList<Obsblock_I>();
		T2obsblock_ = new ArrayList<Obsblock_I>();
		maxSize = 0;
		// count the number of obsblocks in each frequency range
		int omm = 0;
		int tmm = 0;
		int ocm = 0;
		int ftp = 0;
		// convert the projects to a list of obsblocks
		if(projects != null){
			for(int i = 0; i < projects.length; i++){
				for(int j = 0; j < projects[i].obsblock.length; j++){
					if(projects[i].obsblock[j] != null){
						if(projects[i].obsblock[j].exceedTAC){
						    T2obsblock_.add(new Obsblock_I(projects[i].projectID,projects[i].obsblock[j],projects[i].isKeyProject,projects[i].isFastTrack));
						}
						else{
						    Tobsblock_.add(new Obsblock_I(projects[i].projectID,projects[i].obsblock[j],projects[i].isKeyProject,projects[i].isFastTrack));
						}
						if(projects[i].obsblock[j].receiverBand.contains("1MM")){
							omm++;
						}
						else if(projects[i].obsblock[j].receiverBand.contains("3MM")){
							tmm++;
						}
						else{
							ocm++;
						}
						if(projects[i].isFastTrack){
						    ftp++;
						}
						maxSize = Math.max(maxSize, (projects[i].obsblock[j].obsblockID.length() +6));
					}
				}
			}
		}
		jtp.onefield.setText("" + omm);
		jtp.threefield.setText("" + tmm);
		jtp.onecmfield.setText("" + ocm);
		jtp.fastTrackfield.setText("" + ftp);
		jtp.allfield.setText("" + (tmm+omm+ocm));
		obsblock_ = (Obsblock_I[])Tobsblock_.toArray(new Obsblock_I[Tobsblock_.size()]);
		if(T2obsblock_ != null){
			Eobsblock_ = (Obsblock_I[])T2obsblock_.toArray(new Obsblock_I[T2obsblock_.size()]);
		}
		if(sortType == SortType.PRIORITY){
			Arrays.sort(obsblock_,new PriorityCompare());
			if(Eobsblock_ != null){
				Arrays.sort(Eobsblock_,new PriorityCompare());
			}
		}
		else if(sortType == SortType.LST){
			Arrays.sort(obsblock_,new LSTCompare());
			if(Eobsblock_ != null){
				Arrays.sort(Eobsblock_,new LSTCompare());
			}
		}
		else if(sortType == SortType.RA){
			Arrays.sort(obsblock_,new RACompare());
			if(Eobsblock_ != null){
				Arrays.sort(Eobsblock_,new RACompare());
			}
		}
		else if(sortType == SortType.TIME){
			Arrays.sort(obsblock_,new RACompare());
			if(Eobsblock_ != null){
				Arrays.sort(Eobsblock_,new TimeLeftCompare());
			}
		}
		else if(sortType == SortType.RPRIORITY){
			Arrays.sort(obsblock_,new RPriorityCompare());
			if(Eobsblock_ != null){
				Arrays.sort(Eobsblock_,new RPriorityCompare());
			}
		}
		else if(sortType == SortType.RLST){
			Arrays.sort(obsblock_,new RLSTCompare());
			if(Eobsblock_ != null){
				Arrays.sort(Eobsblock_,new RLSTCompare());
			}
		}
		else if(sortType == SortType.RRA){
			Arrays.sort(obsblock_,new RRACompare());
			if(Eobsblock_ != null){
				Arrays.sort(Eobsblock_,new RRACompare());
			}
		}
		else if(sortType == SortType.RTIME){
			Arrays.sort(obsblock_,new RLSTCompare());
			if(Eobsblock_ != null){
				Arrays.sort(Eobsblock_,new RTimeLeftCompare());
			}
		}

		setLayout(new GridBagLayout());
		makePanel();
		// labels for columns
		add(new JLabel("Obsblock ID                                 Band     Time Left          RA             LST Range       Type     Flex      Scal    Key      Script         0       4       8       12      16      20 (LST)"), new GBC(0,0).setFill(GBC.HORIZONTAL).setWeight(100,100));
		JScrollPane scrollPane = new JScrollPane(resultsList);
		scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);		
		scrollPane.setPreferredSize(new Dimension(Globals.WIDTH,465));
		scrollPane.setMinimumSize(new Dimension(Globals.WIDTH,465));
		add(scrollPane, new GBC(0,1).setFill(GBC.BOTH).setWeight(100,100));
	}

	/**
	 * method to create the panel
	 */
	private void makePanel(){
		resultsList = new JPanel();
		resultsList.setLayout(new GridBagLayout());
		int m = 0;
		String stringFormat = "%-" + maxSize + "s";
		for(Obsblock_I obsblock : obsblock_){
		    if((obsblock.obsblock_.receiverBand.contains(band_) || band_.contains("ALL")) || (obsblock.isFastTrack && band_.contains("FT"))){
				JPanel tempPanel = new JPanel();
				// alternate the colors of each line to make them easier to track
				if((m%2) == 0){
				    if(obsblock.isFastTrack){
					tempPanel.setBackground(Color.ORANGE.darker());
				    }
				    else{
					tempPanel.setBackground(Color.LIGHT_GRAY);
				    }
				}
				else{
				    if(obsblock.isFastTrack){
					tempPanel.setBackground(Color.ORANGE.brighter());
				    }
				    else{
					tempPanel.setBackground(Color.WHITE);
				    }
				}
				// put in the different components
				JLabel obsLabel = new JLabel(String.format(stringFormat,obsblock.pid_ + "." + obsblock.obsblock_.obsblockID));
				obsLabel.setPreferredSize(new Dimension(170,YSIZE));
				obsLabel.setMinimumSize(new Dimension(170,YSIZE));
				obsLabel.setMaximumSize(new Dimension(170,YSIZE));
				obsLabel.setToolTipText("Obsblock name <project>.<obsblock>");

				JLabel bandLabel = new JLabel(obsblock.obsblock_.receiverBand);
				bandLabel.setPreferredSize(new Dimension(30,YSIZE));
				bandLabel.setMinimumSize(new Dimension(30,YSIZE));
				bandLabel.setMaximumSize(new Dimension(30,YSIZE));
				bandLabel.setToolTipText("Observing band");
		
				NumberFormat timeFormat = new DecimalFormat("##0.0");
				JLabel hrsLeft = new JLabel(timeFormat.format(obsblock.obsblock_.remainingTime));
				hrsLeft.setPreferredSize(new Dimension(30,YSIZE));
				hrsLeft.setMinimumSize(new Dimension(30,YSIZE));
				hrsLeft.setMaximumSize(new Dimension(30,YSIZE));
				hrsLeft.setToolTipText("Hours remaining to be observed");

				JLabel raLabel = new JLabel(Conversions.RadiansToTime(obsblock.obsblock_.subObsblock[0].trial[obsblock.obsblock_.subObsblock[0].trial.length - 1].source[0].ra));
				raLabel.setPreferredSize(new Dimension(35,YSIZE));
				raLabel.setMinimumSize(new Dimension(35,YSIZE));
				raLabel.setMaximumSize(new Dimension(35,YSIZE));
				raLabel.setToolTipText("Source RA");
		
				JLabel coverLabel = new JLabel(String.format("%5s", Conversions.RadiansToTime(obsblock.obsblock_.lowRa)) + "-" + String.format("%5s",Conversions.RadiansToTime(obsblock.obsblock_.highRa)));
				coverLabel.setPreferredSize(new Dimension(80,YSIZE));
				coverLabel.setMinimumSize(new Dimension(80,YSIZE));
				coverLabel.setMaximumSize(new Dimension(80,YSIZE));
				coverLabel.setToolTipText("LST Range during which the source is observable");
		
				
				JLabel typeLabel = null;
				if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_CARMA15){
					typeLabel = new JLabel(String.format("%3s", "C15"));
					typeLabel.setToolTipText("CARMA 15");
				}
				else if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_CARMA8){
					typeLabel = new JLabel(String.format("%3s", "CA8"));
					typeLabel.setToolTipText("CARMA 8");
				}
				else if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_CARMA23){
					typeLabel = new JLabel(String.format("%3s", "C23"));
					typeLabel.setToolTipText("CARMA 23");
				}
				else if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_PACS){
					typeLabel = new JLabel(String.format("%3s", "PAC"));
					typeLabel.setToolTipText("PACS");
				}
				else if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_SINGLEPOL){
					typeLabel = new JLabel(String.format("%3s", "SPL"));
					typeLabel.setToolTipText("Single Polarization");
				}
				else if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_DUALPOL){
					typeLabel = new JLabel(String.format("%3s", "DPL"));
					typeLabel.setToolTipText("Dual Polarization");
				}
				else if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_FULLPOL){
					typeLabel = new JLabel(String.format("%3s", "FPL"));
					typeLabel.setToolTipText("Full Polarization");
				}
				else if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_MAXSENS_DUALPOL){
					typeLabel = new JLabel(String.format("%3s", "MSP"));
					typeLabel.setToolTipText("Maximum Sensitivity Dual Polarization");
				}
				else if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_MAXSENS_CARMA23){
					typeLabel = new JLabel(String.format("%3s", "MSC"));
					typeLabel.setToolTipText("Maximum Sensitivity CARMA 23");
				}
				else if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_MAXSENS_LL){
					typeLabel = new JLabel(String.format("%3s", "MSL"));
					typeLabel.setToolTipText("Maximum Sensitivity LL");
				}
				else{
					typeLabel = new JLabel(String.format("%3s", "SPL"));
					typeLabel.setToolTipText("Single Polarization");
				}
				typeLabel.setPreferredSize(new Dimension(30,YSIZE));
				typeLabel.setMinimumSize(new Dimension(30,YSIZE));
				typeLabel.setMaximumSize(new Dimension(30,YSIZE));
				
				tempPanel.setLayout(new GridBagLayout());
				tempPanel.add(obsLabel, new GBC(0,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100,100).setIpad(0, 0));
				tempPanel.add(bandLabel, new GBC(1,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100,100).setIpad(0, 0));
				tempPanel.add(hrsLeft, new GBC(2,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100,100).setIpad(0, 0));
				tempPanel.add(raLabel, new GBC(3,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
				tempPanel.add(coverLabel,new GBC(4,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
				tempPanel.add(typeLabel,new GBC(5,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
				if(obsblock.obsblock_.isFlex){
					GreenBox gb = new GreenBox();
					gb.setPreferredSize(new Dimension(10,YSIZE));
					gb.setMinimumSize(new Dimension(10,YSIZE));
					gb.setMaximumSize(new Dimension(10,YSIZE));
					gb.setToolTipText("This obsblock is a flexHA obsblock");
					tempPanel.add(gb,new GBC(6,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
				}
				else{
					RedBox rb = new RedBox();
					rb.setPreferredSize(new Dimension(10,YSIZE));
					rb.setMinimumSize(new Dimension(10,YSIZE));
					rb.setMaximumSize(new Dimension(10,YSIZE));
					rb.setToolTipText("This obsblock is not a flexHA obsblock");
					tempPanel.add(rb,new GBC(6,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
				}
				if(obsblock.obsblock_.subObsblock[0].trial[obsblock.obsblock_.subObsblock[0].trial.length - 1].source[0].isSelfcalibratable){
					GreenBox gb = new GreenBox();
					gb.setPreferredSize(new Dimension(10,YSIZE));
					gb.setMinimumSize(new Dimension(10,YSIZE));
					gb.setMaximumSize(new Dimension(10,YSIZE));
					gb.setToolTipText("This is a selfcalibratable source");
					tempPanel.add(gb,new GBC(7,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
				}
				else{
					RedBox rb = new RedBox();
					rb.setPreferredSize(new Dimension(10,YSIZE));
					rb.setMinimumSize(new Dimension(10,YSIZE));
					rb.setMaximumSize(new Dimension(10,YSIZE));
					rb.setToolTipText("This is not a selfcalibratable source");
					tempPanel.add(rb,new GBC(7,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
				}
				if(obsblock.isKeyProject){
					GreenBox gb = new GreenBox();
					gb.setPreferredSize(new Dimension(10,YSIZE));
					gb.setMinimumSize(new Dimension(10,YSIZE));
					gb.setMaximumSize(new Dimension(10,YSIZE));
					gb.setToolTipText("This is a key project");
					tempPanel.add(gb,new GBC(8,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
				}
				else{
					RedBox rb = new RedBox();
					rb.setPreferredSize(new Dimension(10,YSIZE));
					rb.setMinimumSize(new Dimension(10,YSIZE));
					rb.setMaximumSize(new Dimension(10,YSIZE));
					rb.setToolTipText("This is not a key project");
					tempPanel.add(rb,new GBC(8,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
				}
				FileSystemView fsv = FileSystemView.getFileSystemView();
				File[] filesSci1 = fsv.getFiles(new File(PdbiConfig.getScriptLocSci1()), true);
				File[] filesSci2 = fsv.getFiles(new File(PdbiConfig.getScriptLocSci2()), true);
				File[] filesFT = fsv.getFiles(new File(PdbiConfig.getScriptLocFT()), true);
				File[] files = new File[filesSci1.length + filesSci2.length + filesFT.length];
				System.arraycopy(filesSci1, 0, files, 0, filesSci1.length);
				System.arraycopy(filesSci2, 0, files, filesSci1.length, filesSci2.length);
				System.arraycopy(filesFT, 0, files, filesSci1.length + filesSci2.length, filesFT.length);

				boolean found = false;
				for(File file : files){
					if(file.getName().contains(obsblock.pid_) && file.getName().contains(obsblock.obsblock_.obsblockID) && file.getName().contains(".obs")){
						found = true;
					}
				}
				if(found){
					GreenBox gb = new GreenBox();
					gb.setPreferredSize(new Dimension(10,YSIZE));
					gb.setMinimumSize(new Dimension(10,YSIZE));
					gb.setMaximumSize(new Dimension(10,YSIZE));
					gb.setToolTipText("The script was found in the scripts directory");
					tempPanel.add(gb,new GBC(9,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
				}
				else{
					RedBox rb = new RedBox();
					rb.setPreferredSize(new Dimension(10,YSIZE));
					rb.setMinimumSize(new Dimension(10,YSIZE));
					rb.setMaximumSize(new Dimension(10,YSIZE));
					rb.setToolTipText("The script was not found in the scripts directory");
					tempPanel.add(rb,new GBC(9,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
				}

				PlotCoverage pc = new PlotCoverage(obsblock.obsblock_.lowRa,obsblock.obsblock_.highRa,obsblock.obsblock_.subObsblock[0].trial[obsblock.obsblock_.subObsblock[0].trial.length - 1].source[0].ra,obsblock.obsblock_.actualHourAngleCoverage.trim());
				pc.setPreferredSize(new Dimension(220,YSIZE));
				pc.setMinimumSize(new Dimension(220,YSIZE));
				pc.setMaximumSize(new Dimension(220,YSIZE));
				pc.setToolTipText("Obsblock " + obsblock.pid_ + "." + obsblock.obsblock_.obsblockID + pc.getToolTip());
				tempPanel.add(pc,new GBC(10,0).setFill(GBC.BOTH).setWeight(100, 100).setIpad(0, 0));
				resultsList.add(tempPanel, new GBC(0,m).setFill(GBC.BOTH).setAnchor(GBC.WEST).setWeight(100,100).setWeight(100,100));
				m++;
			}
		}
		if(Eobsblock_ != null){
			if(Eobsblock_.length > 0){
				JPanel tempPanel = new JPanel();
				tempPanel.setBackground(Color.BLACK);
				resultsList.add(tempPanel, new GBC(0,m).setFill(GBC.BOTH).setAnchor(GBC.WEST).setWeight(100,100));
				m++;
			}
			for(Obsblock_I obsblock : Eobsblock_){
				if(obsblock.obsblock_.receiverBand.contains(band_) || band_.contains("ALL")){
					JPanel tempPanel = new JPanel();
					// alternate the colors of each line to make them easier to track
					if((m%2) == 0){
						tempPanel.setBackground(Color.LIGHT_GRAY);
					}
					else{
						tempPanel.setBackground(Color.WHITE);
					}
					// put in the different components
					JLabel obsLabel = new JLabel(String.format(stringFormat,obsblock.pid_ + "." + obsblock.obsblock_.obsblockID));
					obsLabel.setPreferredSize(new Dimension(170,YSIZE));
					obsLabel.setMinimumSize(new Dimension(170,YSIZE));
					obsLabel.setMaximumSize(new Dimension(170,YSIZE));
					obsLabel.setToolTipText("Obsblock name <project>.<obsblock>");

					JLabel bandLabel = new JLabel(obsblock.obsblock_.receiverBand);
					bandLabel.setPreferredSize(new Dimension(30,YSIZE));
					bandLabel.setMinimumSize(new Dimension(30,YSIZE));
					bandLabel.setMaximumSize(new Dimension(30,YSIZE));
					bandLabel.setToolTipText("Observing band");
			
					NumberFormat timeFormat = new DecimalFormat("##0.0");
					JLabel hrsLeft = new JLabel(timeFormat.format(obsblock.obsblock_.remainingTime));
					hrsLeft.setPreferredSize(new Dimension(30,YSIZE));
					hrsLeft.setMinimumSize(new Dimension(30,YSIZE));
					hrsLeft.setMaximumSize(new Dimension(30,YSIZE));
					hrsLeft.setToolTipText("Hours remaining to be observed");

					JLabel raLabel = new JLabel(Conversions.RadiansToTime(obsblock.obsblock_.subObsblock[0].trial[obsblock.obsblock_.subObsblock[0].trial.length - 1].source[0].ra));
					raLabel.setPreferredSize(new Dimension(35,YSIZE));
					raLabel.setMinimumSize(new Dimension(35,YSIZE));
					raLabel.setMaximumSize(new Dimension(35,YSIZE));
					raLabel.setToolTipText("Source RA");
			
					JLabel coverLabel = new JLabel(String.format("%5s", Conversions.RadiansToTime(obsblock.obsblock_.lowRa)) + "-" + String.format("%5s",Conversions.RadiansToTime(obsblock.obsblock_.highRa)));
					coverLabel.setPreferredSize(new Dimension(80,YSIZE));
					coverLabel.setMinimumSize(new Dimension(80,YSIZE));
					coverLabel.setMaximumSize(new Dimension(80,YSIZE));
					coverLabel.setToolTipText("LST Range during which the source is observable");
			
					JLabel typeLabel = null;
					if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_CARMA15){
						typeLabel = new JLabel(String.format("%3s", "C15"));
						typeLabel.setToolTipText("CARMA 15");
					}
					else if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_CARMA8){
						typeLabel = new JLabel(String.format("%3s", "CA8"));
						typeLabel.setToolTipText("CARMA 8");
					}
					else if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_CARMA23){
						typeLabel = new JLabel(String.format("%3s", "C23"));
						typeLabel.setToolTipText("CARMA 23");
					}
					else if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_PACS){
						typeLabel = new JLabel(String.format("%3s", "PAC"));
						typeLabel.setToolTipText("PACS");
					}
					else if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_SINGLEPOL){
						typeLabel = new JLabel(String.format("%3s", "SPL"));
						typeLabel.setToolTipText("Single Polarization");
					}
					else if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_DUALPOL){
						typeLabel = new JLabel(String.format("%3s", "DPL"));
						typeLabel.setToolTipText("Dual Polarization");
					}
					else if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_FULLPOL){
						typeLabel = new JLabel(String.format("%3s", "FPL"));
						typeLabel.setToolTipText("Full Polarization");
					}
					else if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_MAXSENS_DUALPOL){
						typeLabel = new JLabel(String.format("%3s", "MSP"));
						typeLabel.setToolTipText("Maximum Sensitivity Dual Polarization");
					}
					else if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_MAXSENS_CARMA23){
						typeLabel = new JLabel(String.format("%3s", "MSC"));
						typeLabel.setToolTipText("Maximum Sensitivity CARMA 23");
					}
					else if(obsblock.obsblock_.observationType == carma.observertools.ObsType.TYPE_MAXSENS_LL){
						typeLabel = new JLabel(String.format("%3s", "MSL"));
						typeLabel.setToolTipText("Maximum Sensitivity LL");
					}
					else{
						typeLabel = new JLabel(String.format("%3s", "SPL"));
						typeLabel.setToolTipText("Single Polarization");
					}

					typeLabel.setPreferredSize(new Dimension(30,YSIZE));
					typeLabel.setMinimumSize(new Dimension(30,YSIZE));
					typeLabel.setMaximumSize(new Dimension(30,YSIZE));

					tempPanel.setLayout(new GridBagLayout());
					tempPanel.add(obsLabel, new GBC(0,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100,100).setIpad(0, 0));
					tempPanel.add(bandLabel, new GBC(1,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100,100).setIpad(0, 0));
					tempPanel.add(hrsLeft, new GBC(2,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100,100).setIpad(0, 0));
					tempPanel.add(raLabel, new GBC(3,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
					tempPanel.add(coverLabel,new GBC(4,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
					tempPanel.add(typeLabel,new GBC(5,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
					if(obsblock.obsblock_.isFlex){
						GreenBox gb = new GreenBox();
						gb.setPreferredSize(new Dimension(10,YSIZE));
						gb.setMinimumSize(new Dimension(10,YSIZE));
						gb.setMaximumSize(new Dimension(10,YSIZE));
						gb.setToolTipText("This obsblock is a flexHA obsblock");
						tempPanel.add(gb,new GBC(6,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
					}
					else{
						RedBox rb = new RedBox();
						rb.setPreferredSize(new Dimension(10,YSIZE));
						rb.setMinimumSize(new Dimension(10,YSIZE));
						rb.setMaximumSize(new Dimension(10,YSIZE));
						rb.setToolTipText("This obsblock is not a flexHA obsblock");
						tempPanel.add(rb,new GBC(6,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
					}
					if(obsblock.obsblock_.subObsblock[0].trial[obsblock.obsblock_.subObsblock[0].trial.length - 1].source[0].isSelfcalibratable){
						GreenBox gb = new GreenBox();
						gb.setPreferredSize(new Dimension(10,YSIZE));
						gb.setMinimumSize(new Dimension(10,YSIZE));
						gb.setMaximumSize(new Dimension(10,YSIZE));
						gb.setToolTipText("This is a selfcalibratable source");
						tempPanel.add(gb,new GBC(7,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
					}
					else{
						RedBox rb = new RedBox();
						rb.setPreferredSize(new Dimension(10,YSIZE));
						rb.setMinimumSize(new Dimension(10,YSIZE));
						rb.setMaximumSize(new Dimension(10,YSIZE));
						rb.setToolTipText("This is not a selfcalibratable source");
						tempPanel.add(rb,new GBC(7,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
					}
					if(obsblock.isKeyProject){
						GreenBox gb = new GreenBox();
						gb.setPreferredSize(new Dimension(10,YSIZE));
						gb.setMinimumSize(new Dimension(10,YSIZE));
						gb.setMaximumSize(new Dimension(10,YSIZE));
						gb.setToolTipText("This is a key project");
						tempPanel.add(gb,new GBC(8,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
					}
					else{
						RedBox rb = new RedBox();
						rb.setPreferredSize(new Dimension(10,YSIZE));
						rb.setMinimumSize(new Dimension(10,YSIZE));
						rb.setMaximumSize(new Dimension(10,YSIZE));
						rb.setToolTipText("This is not a key project");
						tempPanel.add(rb,new GBC(8,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
					}

				    FileSystemView fsv = FileSystemView.getFileSystemView();
				    File[] filesSci1 = fsv.getFiles(new File(PdbiConfig.getScriptLocSci1()), true);
				    File[] filesSci2 = fsv.getFiles(new File(PdbiConfig.getScriptLocSci2()), true);
				    File[] files = new File[filesSci1.length + filesSci2.length];
				    System.arraycopy(filesSci1, 0, files, 0, filesSci1.length);
				    System.arraycopy(filesSci2, 0, files, filesSci1.length, filesSci2.length);
					boolean found = false;
					for(File file : files){
						if(file.getName().contains(obsblock.pid_) && file.getName().contains(obsblock.obsblock_.obsblockID) && file.getName().contains(".obs")){
							found = true;
						}
					}
					if(found){
						GreenBox gb = new GreenBox();
						gb.setPreferredSize(new Dimension(10,YSIZE));
						gb.setMinimumSize(new Dimension(10,YSIZE));
						gb.setMaximumSize(new Dimension(10,YSIZE));
						gb.setToolTipText("The script was found in the scripts directory");
						tempPanel.add(gb,new GBC(9,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
					}
					else{
						RedBox rb = new RedBox();
						rb.setPreferredSize(new Dimension(10,YSIZE));
						rb.setMinimumSize(new Dimension(10,YSIZE));
						rb.setMaximumSize(new Dimension(10,YSIZE));
						rb.setToolTipText("The script was not found in the scripts directory");
						tempPanel.add(rb,new GBC(9,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
					}

					PlotCoverage pc = new PlotCoverage(obsblock.obsblock_.lowRa,obsblock.obsblock_.highRa,obsblock.obsblock_.subObsblock[0].trial[obsblock.obsblock_.subObsblock[0].trial.length - 1].source[0].ra,obsblock.obsblock_.actualHourAngleCoverage);
					pc.setPreferredSize(new Dimension(220,YSIZE));
					pc.setMinimumSize(new Dimension(220,YSIZE));
					pc.setMaximumSize(new Dimension(220,YSIZE));
					pc.setToolTipText("Obsblock " + obsblock.pid_ + "." + obsblock.obsblock_.obsblockID + pc.getToolTip());
					tempPanel.add(pc,new GBC(10,0).setFill(GBC.BOTH).setWeight(100, 100).setIpad(0, 0));
					resultsList.add(tempPanel, new GBC(0,m).setFill(GBC.BOTH).setAnchor(GBC.WEST).setWeight(100,100).setWeight(100,100));
					m++;
				}
			}
		}
		// fill out the display if there are less then 10 entries
		if(m < 26){
			for(int i = m; i < 26; i++){
				JPanel tempPanel = new JPanel();
				if((i%2) == 0){
					tempPanel.setBackground(Color.LIGHT_GRAY);
				}
				else{
					tempPanel.setBackground(Color.WHITE);
				}
				resultsList.add(tempPanel, new GBC(0,i).setFill(GBC.BOTH).setAnchor(GBC.WEST).setWeight(100,100));
			}
		}
	}
	
	private ArrayList<Obsblock_I> Tobsblock_ = null;
	private ArrayList<Obsblock_I> T2obsblock_ = null;
	private Obsblock_I[] obsblock_ = null;
	private Obsblock_I[] Eobsblock_ = null;
	private JPanel resultsList = null;
	private String band_ = "ANY";
	private int maxSize = 0;
	private static final int YSIZE = 20;
	public enum SortType{LST,PRIORITY,RA,TIME,RLST,RPRIORITY,RRA,RTIME};
}
