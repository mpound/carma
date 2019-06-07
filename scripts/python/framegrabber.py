#
# $Id: framegrabber.py,v 1.6 2010/09/21 17:42:59 abeard Exp $
#
# This requires the Python Image Library
# make sure that you have built it using the python
# that comes with carma_tools...
#
# Current usage:
# make run-python imr=acc
# >>> import framegrabber as fg
# >>> fg.run("bima1",320,200)
# 
# produces image with 320x200 dimensions, and a "grab" button.
# or
#
# >>> fg.run("bima1")
#
# defaults to 768x480 image.
# available resolutions are: 320x200, 640x400, 768x480
#
# CARMA Frame Grabber interface module
#

from Tkinter import *
import Image, ImageTk
import carmaIni
import carma

global antctl, tkimg, c, d, x, y

def grab():
  global antctl, tkimg, c, d, x, y
#  for i in range(1,2):
  print "hello, getting image..."
  img = getimage(antctl)
  print "hello, got image..."
  print "pasting image..."
  tkimg.paste(ImageTk.PhotoImage(img))
#  print "hello, deleting image..."
#  c.delete(d)
#  print "hello, creating image..."
#  d=c.create_image(x/2+1, y/2+1, image=tkimg)
#    c.pack()
  print "pasted..."

def window():
  r = Tk()
  menubar = Menu(r)
  menubar.add_command(label="Grab", command=grab)
  menubar.add_command(label="Quit", command=r.quit)
  r.config(menu=menubar)
  return r

def antenna(antenna):
  o = "carma." + antenna + "." + carma.antenna.common.OPTICAL_TEL_NAME
  co = carmaIni.getObj( o, carma.antenna.common.OpticalTelControl ) 
  return co


def getimage(grabber):
  rawimg = grabber.grabFrame()
  pilimg = Image.frombuffer( "L", (rawimg.x, rawimg.y), rawimg.opticalData, "raw", "L", 0, 1 )
  return pilimg

def run(name,myx=768,myy=480):
  global antctl, tkimg, c, d, x, y
  antctl = antenna(name)
  antctl.setFrameDimensions(myx,myy)
  root = window()
  img = getimage(antctl)
  x, y = img.size
  c = Canvas(root, width=x, height=y, bg="blue")
  tkimg = ImageTk.PhotoImage( img )
  d=c.create_image(x/2+1, y/2+1, image=tkimg)
  c.pack()
  root.mainloop()
