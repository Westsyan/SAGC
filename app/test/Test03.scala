package test

import java.io.File
import java.nio.file.Files

import org.apache.commons.io.FileUtils

object Test03 {

  def main(args: Array[String]): Unit = {

    val tmpDir = Files.createTempDirectory("tmpDir").toString
    val path = tmpDir.replaceAll("\\\\", "/")
    val outputPath = "output.txt"
/*    val rStr =
      s"""
         |setwd('E:\\\\perl')
         |data <-read.table('data.txt',sep="\t",header=T,row.names=1)
         |da <- scale(data,center=T,scale=T)
         |da<-t(da)
         |da <- na.omit(da)
         |da<-t(da)
         |write.table(da,'output.txt',sep='\t',quote=F)
      """.stripMargin
    FileUtils.writeStringToFile(new File("E:\\perl","cmd.r"),rStr)*/

    val pyStr =
      s"""
         |# coding=UTF-8
         |import plotly
         |import plotly.figure_factory as ff
         |from itertools import islice
         |import numpy as np
         |import os
         |os.chdir("E:\\\\perl")
         |data_lines = []
         |input_file = open("output")
         |for line in islice(input_file, 1, None):
         |    line = line.strip("\\n").split("\\t")
         |    del line[0]
         |    for index in range(len(line)):
         |        line[index]=float(line[index])
         |    data_lines.append(line)
         |input_file.close()
         |X = np.mat(data_lines)
         |names=A001	A003	A039	A065	A078	A091	B001	B002	B003	B036	B039
         |fig = ff.create_dendrogram(X, orientation='left',labels=names)
         |fig['layout'].update({'width':1160, 'height':450,'yaxis':{'fixedrange':True},'xaxis':{'fixedrange':True}})
         |fig['data'].update({'name':""})
         |config={}
         |config['displayModeBar']=False
         |a=plotly.offline.plot(fig, filename='simple_dendrogram.html',output_type='div',include_plotlyjs=False,show_link=False,config=config)
         |f=open('div.txt','w')
         |f.write(a)
         |f.close()
      """.stripMargin
    FileUtils.writeStringToFile(new File("E:\\perl","tmp.py"),pyStr)
  }

}
