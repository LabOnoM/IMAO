Acute_angle<-function(X){
  if(X>90){X<-180-X}
  return(X)
}
read.hoc<-function(X){
  print(X)
  a<-readLines(X)
  b<-sum(grepl("create filament_1", a, fixed = TRUE))-1
  endline<-which(a=="}")
  Coordinate<-list()
  for (j in 0:b) {
    templs<-list()
    number<-as.numeric(str_split(a[grepl(paste0("create filament_1", sprintf("%08d", j)), a, fixed = TRUE)], "\\[|\\]")[[1]][2])
    for (i in 0:(number-1)) {
      temp<-paste0("filament_1", sprintf("%08d", j), "[", i, "]", " {")
      row<-which(a==temp)
      start<-a[row+2]
      end<-a[endline[which(endline>row)[1]]-1]
      temp2<-as.numeric(str_split(start, "\\(|\\,")[[1]][2:4])
      temp3<-as.numeric(str_split(end, "\\(|\\,")[[1]][2:4])
      templs[[i+1]]<-list(start=temp2, end=temp3)
      names(templs)[i+1]<-i
    }
    Coordinate[[j+1]]<-templs
    names(Coordinate)[j+1]<-paste0("create filament_1", sprintf("%08d", j))
  }
  return(Coordinate)
}
integrate.results<-function(X){
  print(paste0(X,suffixN))
  a<-read.csv(paste0(X,suffixN,"_Statistics//",X,suffixN,"_Center_of_Homogeneous_Mass.csv"),
              header = TRUE, skip=3)
  a<-a[,-9]
  names(a)[8]<-"SurfaceID"
  b<-read.csv(paste0(X,suffixF,"_Statistics//",X,suffixF,"_Filament_Position.csv"),
              header = TRUE, skip=3)
  names(b)[9]<-"SurfaceID"
  names(b)[8]<-"FilamentID"
  SAA<-read.csv(paste0(X,suffixN,"_Statistics//",X,suffixN,"_Ellipsoid_Axis_A.csv"),
                header = TRUE, skip=3)
  SAA<-SAA[,-9]
  names(SAA)[8]<-"SurfaceID"
  SAB<-read.csv(paste0(X,suffixN,"_Statistics//",X,suffixN,"_Ellipsoid_Axis_B.csv"),
                header = TRUE, skip=3)
  SAB<-SAB[,-9]
  names(SAB)[8]<-"SurfaceID"
  SAC<-read.csv(paste0(X,suffixN,"_Statistics//",X,suffixN,"_Ellipsoid_Axis_C.csv"),
                header = TRUE, skip=3)
  SAC<-SAC[,-9]
  names(SAC)[8]<-"SurfaceID"
  SALA<-read.csv(paste0(X,suffixN,"_Statistics//",X,suffixN,"_Ellipsoid_Axis_Length_A.csv"),
                 header = TRUE, skip=3)
  SALA<-SALA[,-6]
  names(SALA)[5]<-"SurfaceID"
  SALB<-read.csv(paste0(X,suffixN,"_Statistics//",X,suffixN,"_Ellipsoid_Axis_Length_B.csv"),
                 header = TRUE, skip=3)
  SALB<-SALB[,-6]
  names(SALB)[5]<-"SurfaceID"
  SALC<-read.csv(paste0(X,suffixN,"_Statistics//",X,suffixN,"_Ellipsoid_Axis_Length_C.csv"),
                 header = TRUE, skip=3)
  SALC<-SALC[,-6]
  names(SALC)[5]<-"SurfaceID"
  EllipsoidO<-read.csv(paste0(X,suffixN,"_Statistics//",X,suffixN,"_Ellipticity_(oblate).csv"),
                 header = TRUE, skip=3)
  EllipsoidO<-EllipsoidO[,-6]
  names(EllipsoidO)[5]<-"SurfaceID"
  Ellipsoidp<-read.csv(paste0(X,suffixN,"_Statistics//",X,suffixN,"_Ellipticity_(prolate).csv"),
                       header = TRUE, skip=3)
  Ellipsoidp<-Ellipsoidp[,-6]
  names(Ellipsoidp)[5]<-"SurfaceID"
  Spher<-read.csv(paste0(X,suffixN,"_Statistics//",X,suffixN,"_Sphericity.csv"),
                 header = TRUE, skip=3)
  Spher<-Spher[,-6]
  names(Spher)[5]<-"SurfaceID"
  TerPt<-read.csv(paste0(X,suffixF,"_Statistics//",
                         X,suffixF,"_Filament_No._Segment_Terminal_Pts.csv"),
                  header = TRUE, skip=3)
  TerPt<-TerPt[,-6]
  names(TerPt)[5]<-"FilamentID"
  Volume<-read.csv(paste0(X,suffixN,"_Statistics//",X,suffixN,"_Volume.csv"),
                 header = TRUE, skip=3)
  Volume<-Volume[,-6]
  names(Volume)[5]<-"SurfaceID"
  for (i in 1:nrow(b)) {
    b$SurfaceID[i]<-  which.min(
      sqrt(
        (b$Filament.Position.X[i]-a$Center.of.Homogeneous.Mass.X)^2+
          (b$Filament.Position.Y[i]-a$Center.of.Homogeneous.Mass.Y)^2+
          (b$Filament.Position.Z[i]-a$Center.of.Homogeneous.Mass.Z)^2
      )
    ) %>%
      a[.,8]
  }
  temp<-merge(b,a, by='SurfaceID')
  temp<-merge(temp, SAA[,c(1:3,8)], by='SurfaceID')
  temp<-merge(temp, SAB[,c(1:3,8)], by='SurfaceID')
  temp<-merge(temp, SAC[,c(1:3,8)], by='SurfaceID')
  temp<-merge(temp, SALA[,c(1,5)], by='SurfaceID')
  temp<-merge(temp, SALB[,c(1,5)], by='SurfaceID')
  temp<-merge(temp, SALC[,c(1,5)], by='SurfaceID')
  temp<-merge(temp, EllipsoidO[,c(1,5)], by='SurfaceID')
  temp<-merge(temp, Ellipsoidp[,c(1,5)], by='SurfaceID')
  temp<-merge(temp, Spher[,c(1,5)], by='SurfaceID')
  temp<-merge(temp, TerPt[,c(1,5)], by='FilamentID')
  temp<-merge(temp, Volume[,c(1,5)], by='SurfaceID')
  temp<-temp[order(temp$FilamentID),]
  return(temp)
}
Analysis.Osteocyte.Processes<-function(nameSet,TotalVolume, NecleusLarger){
  results<-list()
  temp<-data.frame()
  for (k in 1:length(nameSet)) {
    results[[k]]<-list(StartEndPt=list(), Results=list(), ResultsMoranI=list())
    names(results)[k]<-nameSet[k]
    results[[k]]$StartEndPt<-read.hoc(paste0(nameSet[k], ".ims.hoc"))
    results[[k]]$Results<-integrate.results(nameSet[k])
    PtPos<-read.csv(paste0(nameSet[k],suffixF,"_Statistics//",
                           nameSet[k],suffixF,"_Pt_Position.csv"),
                    header = TRUE, skip=3)
    PtPos<-PtPos[,-12]
    No<-c()
    No.A<-c()
    No.B<-c()
    No.C<-c()
    No.Degree.X<-c()
    No.Degree.Y<-c()
    No.Degree.Z<-c()
    for (l in 1:nrow(results[[k]]$Results)) {
      O<-c(
        results[[k]]$Results$Center.of.Homogeneous.Mass.X[l],
        results[[k]]$Results$Center.of.Homogeneous.Mass.Y[l],
        results[[k]]$Results$Center.of.Homogeneous.Mass.Z[l]
      )
      axis.a<-c(
        results[[k]]$Results$Ellipsoid.Axis.A.X[l],
        results[[k]]$Results$Ellipsoid.Axis.A.Y[l],
        results[[k]]$Results$Ellipsoid.Axis.A.Z[l]
      )
      axis.b<-c(
        results[[k]]$Results$Ellipsoid.Axis.B.X[l],
        results[[k]]$Results$Ellipsoid.Axis.B.Y[l],
        results[[k]]$Results$Ellipsoid.Axis.B.Z[l]
      )
      axis.c<-c(
        results[[k]]$Results$Ellipsoid.Axis.C.X[l],
        results[[k]]$Results$Ellipsoid.Axis.C.Y[l],
        results[[k]]$Results$Ellipsoid.Axis.C.Z[l]
      )
      axis.length<-c(
        results[[k]]$Results$Ellipsoid.Axis.Length.A[l],
        results[[k]]$Results$Ellipsoid.Axis.Length.B[l],
        results[[k]]$Results$Ellipsoid.Axis.Length.C[l]
      )*NecleusLarger#Nuclear enlarge
      Mt<-matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,-O,1),nrow = 4, ncol = 4)
      Mr<-t(matrix(c(axis.a,0,axis.b,0,axis.c,0,0,0,0,1),nrow = 4, ncol = 4))
      ProcessNo.A<-0
      ProcessNo.B<-0
      ProcessNo.C<-0
      ProcessNo<-0
      for (m in 1:length(results[[k]]$StartEndPt[[l]])) {
        e<-matrix(c(results[[k]]$StartEndPt[[l]][[m]]$start,1), nrow = 4, ncol = 1)
        e<-Mr%*%Mt%*%e
        Pt1<-sqrt(
          1/((1/axis.length[1]^2) + ((e[2]^2)/((e[1]^2)*(axis.length[2]^2))) + ((e[3]^2)/((e[1]^2)*(axis.length[3]^2))))+
            1/(((e[1]^2)/((e[2]^2)*(axis.length[1]^2))) + (1/axis.length[2]^2) + ((e[3]^2)/(e[2]^2)*(axis.length[3]^2)))+
            1/(((e[1]^2)/((e[3]^2)*(axis.length[1]^2))) + ((e[2]^2)/((e[3]^2)*(axis.length[2]^2))) + (1/(axis.length[3]^2)))
        )>=
          sqrt(sum(e[1:3]^2))
        e<-matrix(c(results[[k]]$StartEndPt[[l]][[m]]$end,1), nrow = 4, ncol = 1)
        e<-Mr%*%Mt%*%e
        Pt2<-sqrt(
          1/((1/axis.length[1]^2) + ((e[2]^2)/((e[1]^2)*(axis.length[2]^2))) + ((e[3]^2)/((e[1]^2)*(axis.length[3]^2))))+
            1/(((e[1]^2)/((e[2]^2)*(axis.length[1]^2))) + (1/axis.length[2]^2) + ((e[3]^2)/(e[2]^2)*(axis.length[3]^2)))+
            1/(((e[1]^2)/((e[3]^2)*(axis.length[1]^2))) + ((e[2]^2)/((e[3]^2)*(axis.length[2]^2))) + (1/(axis.length[3]^2)))
        )<=
          sqrt(sum(e[1:3]^2))
        if((Pt1+Pt2)==2){
          ProcessNo<-ProcessNo+1
        }
      }
      No<-c(No,ProcessNo)
      temp.p<-PtPos[PtPos$FilamentID==results[[k]]$Results$FilamentID[l],]
      temp.p<-temp.p[temp.p$Type=="Segment Terminal",]
      for (m in 1:nrow(temp.p)) {
        e<-matrix(c(unlist(temp.p[m,1:3]),1), nrow = 4, ncol = 1)
        e<-Mr%*%Mt%*%e
        if(abs(e[1])>=axis.length[1]){
          ProcessNo.A<-ProcessNo.A+1
        }else if(abs(e[2])>=axis.length[2]){
          ProcessNo.B<-ProcessNo.B+1
        }else if(abs(e[3])>=axis.length[3]){
          ProcessNo.C<-ProcessNo.C+1
        }
      }
      No.A<-c(No.A,ProcessNo.A)
      No.B<-c(No.B,ProcessNo.B)
      No.C<-c(No.C,ProcessNo.C)
      Pos.Axis.C<-c(
        results[[k]]$Results$Ellipsoid.Axis.C.X[l],
        results[[k]]$Results$Ellipsoid.Axis.C.Y[l],
        results[[k]]$Results$Ellipsoid.Axis.C.Z[l]
      )
      Pos.Axis.X<-c(1,0,0)
      Pos.Axis.Y<-c(0,1,0)
      Pos.Axis.Z<-c(0,0,1)
      Degree.Axis.X<-Acute_angle(acos(sum(Pos.Axis.X*Pos.Axis.C)/(sqrt(sum(Pos.Axis.X^2))*sqrt(sum(Pos.Axis.C^2))))*180/pi)
      Degree.Axis.Y<-Acute_angle(acos(sum(Pos.Axis.Y*Pos.Axis.C)/(sqrt(sum(Pos.Axis.Y^2))*sqrt(sum(Pos.Axis.C^2))))*180/pi)
      Degree.Axis.Z<-Acute_angle(acos(sum(Pos.Axis.Z*Pos.Axis.C)/(sqrt(sum(Pos.Axis.Z^2))*sqrt(sum(Pos.Axis.C^2))))*180/pi)
      No.Degree.X<-c(No.Degree.X,Degree.Axis.X)
      No.Degree.Y<-c(No.Degree.Y,Degree.Axis.Y)
      No.Degree.Z<-c(No.Degree.Z,Degree.Axis.Z)
    }
    results[[k]]$Results<-cbind(
      results[[k]]$Results,
      ProcessNo=No,
      ProcessNo.A=No.A,
      ProcessNo.B=No.B,
      ProcessNo.C=No.C,
      Degree.Axis.X=No.Degree.X,
      Degree.Axis.Y=No.Degree.Y,
      Degree.Axis.Z=No.Degree.Z
    )
    results[[k]]$Results<-cbind(
      results[[k]]$Results,
      BrachesNo=results[[k]]$Results$Filament.No..Segment.Terminal.Pts-results[[k]]$Results$ProcessNo
    )
    ExVol<-TotalVolume[k]-sum(results[[k]]$Results$Volume)
    FilaVol<-read.csv(paste0(nameSet[k],suffixC,"_Statistics//",
                             nameSet[k],suffixC,"_Filament_Volume_(sum).csv"),
                      header = TRUE, skip=3)
    FilaVol<-FilaVol[,-6]
    names(FilaVol)[5]<-"FilamentID"
    FilaLen<-read.csv(paste0(nameSet[k],suffixC,"_Statistics//",
                             nameSet[k],suffixC,"_Filament_Length_(sum).csv"),
                      header = TRUE, skip=3)
    FilaLen<-FilaLen[,-6]
    names(FilaLen)[5]<-"FilamentID"
    ozone.dists<-as.matrix(dist(results[[k]]$Results[,10:12]))
    ozone.dists.inv<-1/ozone.dists
    diag(ozone.dists.inv)<-0
    ozone.dists.inv[which(!is.finite(ozone.dists.inv))]<-0
    results[[k]]$ResultsMoranI<-data.frame(
      Measurement_Term=c(
        "Ellipticity (oblate)",
        "Ellipticity (prolate)",
        "Sphericity",
        "Terminal No.",
        "Cellular Volume",
        "Processes No.",
        "Braches No.",
        "Processes No.A",
        "Processes No.B",
        "Processes No.C",
        "Degree.Axis.X",
        "Degree.Axis.Y",
        "Degree.Axis.Z"
      ),
      Moran_I=c(
        Moran.I(results[[k]]$Results$Ellipticity..oblate.,ozone.dists.inv)$observed,
        Moran.I(results[[k]]$Results$Ellipticity..prolate.,ozone.dists.inv)$observed,
        Moran.I(results[[k]]$Results$Sphericity,ozone.dists.inv)$observed,
        Moran.I(results[[k]]$Results$Filament.No..Segment.Terminal.Pts,ozone.dists.inv)$observed,
        Moran.I(results[[k]]$Results$Volume,ozone.dists.inv)$observed,
        Moran.I(results[[k]]$Results$ProcessNo,ozone.dists.inv)$observed,
        Moran.I(results[[k]]$Results$BrachesNo,ozone.dists.inv)$observed,
        Moran.I(results[[k]]$Results$ProcessNo.A,ozone.dists.inv)$observed,
        Moran.I(results[[k]]$Results$ProcessNo.B,ozone.dists.inv)$observed,
        Moran.I(results[[k]]$Results$ProcessNo.C,ozone.dists.inv)$observed,
        Moran.I(results[[k]]$Results$Degree.Axis.X,ozone.dists.inv)$observed,
        Moran.I(results[[k]]$Results$Degree.Axis.Y,ozone.dists.inv)$observed,
        Moran.I(results[[k]]$Results$Degree.Axis.Z,ozone.dists.inv)$observed
      ),
      Pvalue_for_Moran_I=c(
        Moran.I(results[[k]]$Results$Ellipticity..oblate.,ozone.dists.inv)$p.value,
        Moran.I(results[[k]]$Results$Ellipticity..prolate.,ozone.dists.inv)$p.value,
        Moran.I(results[[k]]$Results$Sphericity,ozone.dists.inv)$p.value,
        Moran.I(results[[k]]$Results$Filament.No..Segment.Terminal.Pts,ozone.dists.inv)$p.value,
        Moran.I(results[[k]]$Results$Volume,ozone.dists.inv)$p.value,
        Moran.I(results[[k]]$Results$ProcessNo,ozone.dists.inv)$p.value,
        Moran.I(results[[k]]$Results$BrachesNo,ozone.dists.inv)$p.value,
        Moran.I(results[[k]]$Results$ProcessNo.A,ozone.dists.inv)$p.value,
        Moran.I(results[[k]]$Results$ProcessNo.B,ozone.dists.inv)$p.value,
        Moran.I(results[[k]]$Results$ProcessNo.C,ozone.dists.inv)$p.value,
        Moran.I(results[[k]]$Results$Degree.Axis.X,ozone.dists.inv)$p.value,
        Moran.I(results[[k]]$Results$Degree.Axis.Y,ozone.dists.inv)$p.value,
        Moran.I(results[[k]]$Results$Degree.Axis.Z,ozone.dists.inv)$p.value
      )
    )
    temp2<-data.frame(
      SampleID=nameSet[k],
      Volume_Osteocyte_to_Total=sum(results[[k]]$Results$Volume)/TotalVolume[k],
      OsteocyteNo.Per_Cubic_Millimeter=nrow(results[[k]]$Results)/(TotalVolume[k]/(1e9)),
      Volume_Processes_to_Total=FilaVol$Filament.Volume..sum./ExVol,
      Length_Per_Cubic_Micrometer=FilaLen$Filament.Length..sum./ExVol,
      Processes_Volume_Per_Cell=FilaVol$Filament.Volume..sum./nrow(results[[k]]$Results),
      Processes_Length_Per_Cell=FilaLen$Filament.Length..sum./nrow(results[[k]]$Results)
    )
    if(k==1){temp<-temp2}else{temp<-rbind(temp,temp2)}
  }
  results<-c(results,list(ResultsSummary=temp))
  return(results)
}
ExcelXlsxOut<-function(Results){
  xlsx<-createWorkbook()
  addWorksheet(xlsx, "Individual_Osteocyte")
  addWorksheet(xlsx, "Sample_Summary")
  addWorksheet(xlsx, "Moran's_Index")
  GroupID.ls<-c()
  SampleID.ls<-c()
  writeData(xlsx, 1, "Group ID", startCol = 1, startRow = 1)
  writeData(xlsx, 1, "Sample ID", startCol = 2, startRow = 1)
  FilamentID<-c()
  NucleusID<-c()
  EO<-c()
  EP<-c()
  Sphericity<-c()
  NucleusVolume<-c()
  TerminalPtsNo<-c()
  ProcessesNo<-c()
  BrachesNo<-c()
  ProcessesNo.A<-c()
  ProcessesNo.B<-c()
  ProcessesNo.C<-c()
  Degree.Axis.X<-c()
  Degree.Axis.Y<-c() 
  Degree.Axis.Z<-c()
  EllipsoidAxisLA<-c()
  EllipsoidAxisLB<-c()
  EllipsoidAxisLC<-c()
  MoranIndex<-c()
  rowsStart<-2
  for (i in 1:length(GroupReplicates)) {
    if(i==1){
      index<-1
    }else{
      index<-sum(GroupReplicates[1:(i-1)])+1
    }
    rowsEnd<-0
    rowsStart.2<-rowsStart
    for (j in index:sum(GroupReplicates[1:i])) {
      rowsEnd<-rowsEnd+nrow(Results[[j]]$Results)
      writeData(xlsx, 1, names(Results)[j], startCol = 2, startRow = rowsStart.2)
      mergeCells(xlsx, 1, cols=2, rows=rowsStart.2:(sum(rowsStart.2, nrow(Results[[j]]$Results))-1))
      rowsStart.2<-sum(rowsStart.2, nrow(Results[[j]]$Results))
      FilamentID<-c(FilamentID, Results[[j]]$Results$FilamentID)
      NucleusID<-c(NucleusID, Results[[j]]$Results$SurfaceID)
      EO<-c(EO, Results[[j]]$Results$Ellipticity..oblate.)
      EP<-c(EP, Results[[j]]$Results$Ellipticity..prolate.)
      Sphericity<-c(Sphericity, Results[[j]]$Results$Sphericity)
      NucleusVolume<-c(NucleusVolume, Results[[j]]$Results$Volume)
      TerminalPtsNo<-c(TerminalPtsNo, Results[[j]]$Results$Filament.No..Segment.Terminal.Pts)
      ProcessesNo<-c(ProcessesNo, Results[[j]]$Results$ProcessNo)
      BrachesNo<-c(BrachesNo, Results[[j]]$Results$BrachesNo)
      ProcessesNo.A<-c(ProcessesNo.A, Results[[j]]$Results$ProcessNo.A)
      ProcessesNo.B<-c(ProcessesNo.B, Results[[j]]$Results$ProcessNo.B)
      ProcessesNo.C<-c(ProcessesNo.C, Results[[j]]$Results$ProcessNo.C)
      Degree.Axis.X<-c(Degree.Axis.X, Results[[j]]$Results$Degree.Axis.X)
      Degree.Axis.Y<-c(Degree.Axis.Y, Results[[j]]$Results$Degree.Axis.Y) 
      Degree.Axis.Z<-c(Degree.Axis.Z, Results[[j]]$Results$Degree.Axis.Z)
      EllipsoidAxisLA<-c(EllipsoidAxisLA,Results[[j]]$Results$Ellipsoid.Axis.Length.A)
      EllipsoidAxisLB<-c(EllipsoidAxisLB,Results[[j]]$Results$Ellipsoid.Axis.Length.B)
      EllipsoidAxisLC<-c(EllipsoidAxisLC,Results[[j]]$Results$Ellipsoid.Axis.Length.C)
      temp<-c()
      for (k in 1:nrow(Results[[j]]$ResultsMoranI)) {
        temp<-c(temp,c(Results[[j]]$ResultsMoranI[k,2],Results[[j]]$ResultsMoranI[k,3]))
      }
      MoranIndex<-rbind(MoranIndex,temp)
    }
    writeData(xlsx, 1, GroupID[i], startCol = 1, startRow = rowsStart)
    mergeCells(xlsx, 1, cols=1, rows=rowsStart:(sum(rowsStart, rowsEnd)-1))
    writeData(xlsx, 2, GroupID[i], startCol = 1, startRow = index+1)
    mergeCells(xlsx, 2, cols=1, rows=index:sum(GroupReplicates[1:i])+1)
    writeData(xlsx, 3, GroupID[i], startCol = 1, startRow = index+2)
    mergeCells(xlsx, 3, cols=1, rows=index:sum(GroupReplicates[1:i])+2)
    rowsStart<-sum(rowsStart, rowsEnd)
  }
  ResultsSummary<-data.frame(
    FilamentID=FilamentID,
    NucleusID=NucleusID,
    EO=EO,
    EP=EP,
    Sphericity=Sphericity,
    NucleusVolume=NucleusVolume,
    TerminalPtsNo=TerminalPtsNo,
    ProcessesNo=ProcessesNo,
    BrachesNo=BrachesNo,
    ProcessesNo.A=ProcessesNo.A,
    ProcessesNo.B=ProcessesNo.B,
    ProcessesNo.C=ProcessesNo.C,
    Degree.Axis.X=Degree.Axis.X,
    Degree.Axis.Y=Degree.Axis.Y,
    Degree.Axis.Z=Degree.Axis.Z,
    EllipsoidAxisLA=EllipsoidAxisLA,
    EllipsoidAxisLB=EllipsoidAxisLB,
    EllipsoidAxisLC=EllipsoidAxisLC
  )
  writeData(xlsx, 1, ResultsSummary, startCol = 3, startRow = 1)
  writeData(xlsx, 2, Results$ResultsSummary, startCol = 2, startRow = 1)
  ColName<-c("Filament ID", 
             "Nucleus ID",
             "Ellipticity (Oblate)",
             "Ellipticity (Prolate)",
             "Sphericity",
             "Volume of Nucleus (um3)",
             "Terminal Points No.",
             "Main Processes No.",
             "All Braches No. (Terminal Points No. - Main Proceses No.)",
             "Processes No. Along with the Direction of Axis A of the Best Fitted Ellipsoid",
             "Processes No. Along with the Direction of Axis B of the Best Fitted Ellipsoid",
             "Processes No. Along with the Direction of Axis C of the Best Fitted Ellipsoid",
             "Degree of Axis C to Global Axis X (degree)",
             "Degree of Axis C to Global Axis Y (degree)",
             "Degree of Axis C to Global Axis Z (degree)",
             "Length of Axis A of the Best Fitted Ellipsoid (um)",
             "Length of Axis B of the Best Fitted Ellipsoid (um)",
             "Length of Axis C of the Best Fitted Ellipsoid (um)")
  ColName.2<-c("Group ID",
               "Sample ID", 
               "Volume Ratio (Nucleus to Total)",
               "Osteocytes No. per Cubic Millimeter",
               "Volume Ratio (Processes to Total)",
               "Processes Length per Cubic Micrometer (um/um3)",
               "Processes Volume per Cell (um3/cell)",
               "Processes Length per Cell (um/cell)")
  ColName.3<-c("Group ID",
               "Sample ID", 
               "Ellipticity (Oblate)",
               "Ellipticity (Prolate)",
               "Sphericity",
               "Terminal Points No.",
               "Volume of Nucleus (um3)",
               "Main Processes No.",
               "All Braches No. (Terminal Points No. - Main Proceses No.)",
               "Processes No. Along with the Direction of Axis A of the Best Fitted Ellipsoid",
               "Processes No. Along with the Direction of Axis B of the Best Fitted Ellipsoid",
               "Processes No. Along with the Direction of Axis C of the Best Fitted Ellipsoid",
               "Degree of Axis C to Global Axis X (degree)",
               "Degree of Axis C to Global Axis Y (degree)",
               "Degree of Axis C to Global Axis Z (degree)")
  MoranIndex<-cbind(Results$ResultsSummary$SampleID, MoranIndex)
  MoranIndex<-as.data.frame(MoranIndex)
  MoranIndex[,-1]<-apply(MoranIndex[,-1],2, as.numeric)
  writeData(xlsx, 3, MoranIndex, startCol = 2, startRow = 2)
  for (i in 1:18) {
    writeData(xlsx, 1, ColName[i], startCol = i+2, startRow = 1, withFilter = TRUE)
    style<-createStyle(fgFill="cyan", halign="CENTER", valign = "CENTER", border="bottom", 
                       textDecoration="Bold", wrapText = TRUE)
    addStyle(xlsx, 1, style=style, cols=i+2, rows=1)
    if(i<=8){
      writeData(xlsx, 2, ColName.2[i], startCol = i, startRow = 1, withFilter = TRUE)
      if(i>1){addStyle(xlsx, 2, style=style, cols=i, rows=1)}
    }
    if(i==1){
      writeData(xlsx, 3, "Basic Information", startCol = i, startRow = 1)
      writeData(xlsx, 3, ColName.3[i], startCol = i, startRow = 2, withFilter = TRUE)
    }else if(i==2){
      mergeCells(xlsx, 3, cols=1:2, rows=1)
      writeData(xlsx, 3, ColName.3[i], startCol = i, startRow = 2, withFilter = TRUE)
    }else if(i<=15){
      writeData(xlsx, 3, ColName.3[i], startCol = ((i-3)*2)+3, startRow = 1, withFilter = TRUE)
      writeData(xlsx, 3, "Moran's I", startCol = ((i-3)*2)+3, startRow = 2, withFilter = TRUE)
      writeData(xlsx, 3, "P-value", startCol = ((i-3)*2)+4, startRow = 2, withFilter = TRUE)
      mergeCells(xlsx, 3, cols=(((i-3)*2)+3):(((i-3)*2)+4), rows=1)
      addStyle(xlsx, 3, style=style, cols=(((i-3)*2)+3):(((i-3)*2)+4), rows=1)
    }
  }
  style<-createStyle(fgFill="gray", halign="RIGHT", valign = "CENTER", border="bottom",
                     textDecoration="Bold", wrapText = TRUE)
  addStyle(xlsx, 1, style=style, cols=2, rows = 1:(length(FilamentID)+1))
  addStyle(xlsx, 2, style=style, cols=2, rows = 1:(nrow(Results$ResultsSummary)+1))
  addStyle(xlsx, 3, style=style, cols=2, rows = 1:(nrow(Results$ResultsSummary)+2))
  style<-createStyle(halign="RIGHT", valign = "CENTER", border="bottom",
                     textDecoration="Bold", wrapText = TRUE)
  addStyle(xlsx, 1, style=style, cols=1, rows = 1:(length(FilamentID)+1))
  addStyle(xlsx, 2, style=style, cols=1, rows = 1:(nrow(Results$ResultsSummary)+1))
  addStyle(xlsx, 3, style=style, cols=1, rows = 1:(nrow(Results$ResultsSummary)+2))
  addFilter(xlsx, 1, rows = 1, cols = 1:20)
  addFilter(xlsx, 2, rows = 1, cols = 1:8)
  addFilter(xlsx, 3, rows = 2, cols = 1:28)
  setColWidths(xlsx, 1, cols = 2, widths = 25)
  setColWidths(xlsx, 2, cols = 2, widths = 25)
  setColWidths(xlsx, 3, cols = 2, widths = 25)
  setColWidths(xlsx, 1, cols = 3:20, widths = 15)
  setColWidths(xlsx, 2, cols = 3:8, widths = 15)
  setColWidths(xlsx, 3, cols = 3:28, widths = 9)
  setRowHeights(xlsx, 1, rows = 1, heights = 80)
  setRowHeights(xlsx, 2, rows = 1, heights = 80)
  setRowHeights(xlsx, 3, rows = 1, heights = 80)
  saveWorkbook(xlsx, "Osteocyte.Processes.Analysis.Summary.xlsx", overwrite = TRUE)
}