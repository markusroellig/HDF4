(* Mathematica Package *)

(* Created by the Wolfram Workbench 18.10.2011 *)

BeginPackage["HDF4`"]
(* Exported symbols added here with SymbolName::usage *) 
HDF4Import::usage = 
  "HDF4Import[file,element] imports the wanted element from the HDF4 \
file. Possible elements ares \"Data\" and \"Datasets\". Currently \
HDF4Import only supports Vdata sets.";



Begin["`Private`"]
(* Implementation of the package *)

HDF4Import[file_, elem_: "Data"] := Module[{stream, out},
  If[FileExistsQ[file],
   stream = OpenRead[file, BinaryFormat -> True];
   out = Switch[elem,
     "Datasets", findVdataName[stream, #] & /@ listVdataSets[stream],
     "Data", extractVdata[stream],
     True, extractVdata[stream]];
   Close[stream];
   out,
   Message[Import::nffil]; $Failed]]

getReferenceNumbers[dd_, tag_] := getTags[dd, tag][[All, 2]]
getOffsets[dd_, tag_] := getTags[dd, tag][[All, 3]]
getLengths[dd_, tag_] := getTags[dd, tag][[All, 4]]
getTags[dd_, tag_] := Select[dd, #[[1]] == tag &]
collectReferences[dd_, ref_] := Select[dd, #[[2]] == ref &]
getTagRefOffset[dd_, tag_, ref_] := 
 Select[dd, #[[1 ;; 2]] == {tag, ref} &][[1, 3]]
getTagRefLength[dd_, tag_, ref_] := 
 Select[dd, #[[1 ;; 2]] == {tag, ref} &][[1, 4]]


readDD[stream_] := 
 Module[{tmp}, {tmp = 
     BinaryReadList[stream, "UnsignedInteger16", 1, 
      ByteOrdering -> -$ByteOrdering], 
    If[tmp == {1962}, 
     BinaryReadList[stream, "UnsignedInteger16", 1, 
      ByteOrdering -> -$ByteOrdering], 
     BinaryReadList[stream, "Integer16", 1, 
      ByteOrdering -> -$ByteOrdering]], 
    BinaryReadList[stream, "UnsignedInteger32", 1, 
     ByteOrdering -> -$ByteOrdering], 
    BinaryReadList[stream, "UnsignedInteger32", 1, 
     ByteOrdering -> -$ByteOrdering]}] // Flatten

readDDBlock[stream_, size_, pos_] := 
 Module[{blocksize, blockfield, dataDescriptor},
  SetStreamPosition[stream, pos];
  blocksize = 
   BinaryReadList[stream, "UnsignedInteger16", 1, 
    ByteOrdering -> -$ByteOrdering];
  blockfield = 
   BinaryReadList[stream, "UnsignedInteger32", 1, 
    ByteOrdering -> -$ByteOrdering];
  dataDescriptor = Table[readDD[stream], {size}];
  {First@blocksize, First@blockfield, dataDescriptor}]

readDataDescriptor[stream_] := 
 Module[{ddBlock, numberOfDDs, nblock, DD, initialnblock, 
   max},
  max = SetStreamPosition[stream, -1];
  SetStreamPosition[stream, 4];
  initialnblock = 
   First@BinaryReadList[stream, "UnsignedInteger16", 1, 
     ByteOrdering -> -$ByteOrdering];
  SetStreamPosition[stream, 4];
  {numberOfDDs, nblock, DD} = readDDBlock[stream, initialnblock, 4];
  DD = {DD};
  While[nblock != 0,
   {numberOfDDs, nblock, ddBlock} = 
    readDDBlock[stream, numberOfDDs, nblock];
   AppendTo[DD, ddBlock];
   ];
  DeleteCases[
   DeleteCases[Flatten[DD, 1], {1, 0, __}], {_, _, a_ /; a > max, _}]]

translateNumberType[num_] := 
 Switch[num, 3, "Character8", 4, "Character8", 5, "Real32", 6, 
  "Real64", 20, "Integer8", 21, "UnsignedInteger8", 22, "Integer16", 
  23, "UnsignedInteger16", 24, "Integer32", 25, "UnsignedInteger32", 
  26, "Integer64", 27, "UnsignedInteger64"]

getDFTAGVS[stream_, ref_] := 
 Module[{len, off, dim, DFTAGVH, dataType, records, raw, 
   order},
  len = getVdataLength[stream, ref];
  off = getVdataOffset[stream, ref];
  dim = getVdataDimensions[stream, ref];
  DFTAGVH = getDFTAGVH[stream, ref];
  order = DFTAGVH[[6]];
  dataType = translateNumberType /@ DFTAGVH[[5]];
  records = First[dim];
  SetStreamPosition[stream, off];
  raw = Table[
    MapThread[
     BinaryReadList[stream, #1, #2, 
       ByteOrdering -> -$ByteOrdering] &, {dataType, 
      order}], {records}]
  ]


getDFTAGVH[stream_, ref_] := 
 Module[{interlace, numRec, recSize, nfields, fielddatatype, 
   fieldoffset, fieldorder, fieldnamelen, fieldname, namelen, name, 
   classlen, class, extag, exref, version, fnamelen, flags, numofattr,
    attrindexlist},
  (*DFTAG_VH implicits tag 1962 = Vdata *)
  SetStreamPosition[stream, 
   getTagRefOffset[readDataDescriptor[stream], 1962, ref]];(* 
  2 byte interlace at the beginning*)
  interlace = 
   BinaryRead[stream, "UnsignedInteger16", 
    ByteOrdering -> -$ByteOrdering];
  numRec = 
   BinaryRead[stream, "UnsignedInteger32", 
    ByteOrdering -> -$ByteOrdering];
  recSize = 
   BinaryRead[stream, "UnsignedInteger16", 
    ByteOrdering -> -$ByteOrdering];(*record size (in bytes)*)
  nfields = 
   BinaryRead[stream, "UnsignedInteger16", 
    ByteOrdering -> -$ByteOrdering];
  fielddatatype = 
   BinaryReadList[stream, "UnsignedInteger16", nfields*1, 
    ByteOrdering -> -$ByteOrdering];
  fieldoffset = 
   BinaryReadList[stream, "UnsignedInteger16", nfields*1, 
    ByteOrdering -> -$ByteOrdering];
  BinaryReadList[stream, "UnsignedInteger16", nfields*1, 
   ByteOrdering -> -$ByteOrdering];
  fieldorder = 
   BinaryReadList[stream, "UnsignedInteger16", nfields*1, 
    ByteOrdering -> -$ByteOrdering];
  (*fieldnamelen=BinaryReadList[stream,"UnsignedInteger16",nfields*1,
  ByteOrdering->-$ByteOrdering];*)
  If[nfields == 1,
   fieldnamelen = 
    BinaryReadList[stream, "UnsignedInteger16", nfields*1, 
     ByteOrdering -> -$ByteOrdering];
   fieldname = {StringJoin@
      BinaryReadList[stream, "Character8", First@fieldnamelen, 
       ByteOrdering -> -$ByteOrdering]},
   fieldnamelen = {}; fieldname = {};
   Table[fnamelen = 
     BinaryRead[stream, "UnsignedInteger16", 
      ByteOrdering -> -$ByteOrdering];
    AppendTo[fieldnamelen, fnamelen];
    AppendTo[fieldname, 
     StringJoin@
      BinaryReadList[stream, "Character8", fnamelen, 
       ByteOrdering -> -$ByteOrdering]], {nfields}]];
  namelen = 
   BinaryRead[stream, "UnsignedInteger16", 
    ByteOrdering -> -$ByteOrdering];
  name = StringJoin@
    BinaryReadList[stream, "Character8", namelen, 
     ByteOrdering -> -$ByteOrdering];
  classlen = 
   BinaryRead[stream, "UnsignedInteger16", 
    ByteOrdering -> -$ByteOrdering];
  class = 
   StringJoin@
    BinaryReadList[stream, "Character8", classlen, 
     ByteOrdering -> -$ByteOrdering];
  extag = 
   BinaryRead[stream, "UnsignedInteger16", 
    ByteOrdering -> -$ByteOrdering];
  exref = 
   BinaryRead[stream, "UnsignedInteger16", 
    ByteOrdering -> -$ByteOrdering];
  version = 
   BinaryRead[stream, "UnsignedInteger16", 
    ByteOrdering -> -$ByteOrdering];
  numofattr = 0;
  If[version > 3,
   BinaryRead[stream, "UnsignedInteger16", 
    ByteOrdering -> -$ByteOrdering];
   flags = 
    BinaryRead[stream, "UnsignedInteger32", 
     ByteOrdering -> -$ByteOrdering];
   If[IntegerDigits[flags, 2][[-1]] == 1,
    (* has attributes*)
    numofattr = 
     BinaryRead[stream, "UnsignedInteger32", 
      ByteOrdering -> -$ByteOrdering];
    Print[numofattr];
    attrindexlist = 
     Table[{BinaryRead[stream, "Integer32", 
        ByteOrdering -> -$ByteOrdering], 
       BinaryRead[stream, "UnsignedInteger16", 
        ByteOrdering -> -$ByteOrdering], 
       BinaryRead[stream, "UnsignedInteger16", 
        ByteOrdering -> -$ByteOrdering]}, {numofattr}]];
   ];
  If[numofattr == 0,
   {name, numRec, recSize, nfields, fielddatatype, fieldoffset, 
    fieldorder, fieldnamelen, fieldname, class, version},
   {name, numRec, recSize, nfields, fielddatatype, fieldoffset, 
    fieldorder, fieldnamelen, fieldname, class, version, numofattr, 
    attrindexlist}]]

(*getDFTAGVG::usage = 
  "getDFTAGVG[stream,ref] returns the contents of the DFTAG_VG Vgroup \
descriptor.";*)
getDFTAGVG[stream_, ref_] := 
 Module[{ numRec, 
   namelen, name, 
   classlen, class, extag, exref, version,  flags, numofattr,
    attrindexlist, taglist, reflist},
  (*DFTAG_VH implicits tag 1962 = Vdata *)
  SetStreamPosition[stream, 
   getTagRefOffset[readDataDescriptor[stream], 1965, ref]];(* 
  2 byte interlace at the beginning*)
  numRec = 
   BinaryRead[stream, "UnsignedInteger16", 
    ByteOrdering -> -$ByteOrdering];
  Print[numRec];
  If[numRec == 1,
   taglist = 
    BinaryRead[stream, "UnsignedInteger16", 
     ByteOrdering -> -$ByteOrdering],
   taglist = 
    BinaryReadList[stream, "UnsignedInteger16", numRec, 
     ByteOrdering -> -$ByteOrdering]];
  If[numRec == 1,
   reflist = 
    BinaryRead[stream, "UnsignedInteger16", 
     ByteOrdering -> -$ByteOrdering],
   reflist = 
    BinaryReadList[stream, "UnsignedInteger16", numRec, 
     ByteOrdering -> -$ByteOrdering]
   ];
  namelen = 
   BinaryRead[stream, "UnsignedInteger16", 
    ByteOrdering -> -$ByteOrdering];
  name = StringJoin@
    BinaryReadList[stream, "Character8", namelen, 
     ByteOrdering -> -$ByteOrdering];
  classlen = 
   BinaryRead[stream, "UnsignedInteger16", 
    ByteOrdering -> -$ByteOrdering];
  class = 
   StringJoin@
    BinaryReadList[stream, "Character8", classlen, 
     ByteOrdering -> -$ByteOrdering];
  extag = 
   BinaryRead[stream, "UnsignedInteger16", 
    ByteOrdering -> -$ByteOrdering];
  exref = 
   BinaryRead[stream, "UnsignedInteger16", 
    ByteOrdering -> -$ByteOrdering];
  version = 
   BinaryRead[stream, "UnsignedInteger16", 
    ByteOrdering -> -$ByteOrdering];
  numofattr = 0;
  If[version > 3,
   BinaryRead[stream, "UnsignedInteger16", 
    ByteOrdering -> -$ByteOrdering];
   flags = 
    BinaryRead[stream, "UnsignedInteger32", 
     ByteOrdering -> -$ByteOrdering];
   If[IntegerDigits[flags, 2][[-1]] == 1,
    (* has attributes*)
    numofattr = 
     BinaryRead[stream, "UnsignedInteger32", 
      ByteOrdering -> -$ByteOrdering];
    Print[numofattr];
    attrindexlist = 
     Table[{BinaryRead[stream, "Integer32", 
        ByteOrdering -> -$ByteOrdering], 
       BinaryRead[stream, "UnsignedInteger16", 
        ByteOrdering -> -$ByteOrdering], 
       BinaryRead[stream, "UnsignedInteger16", 
        ByteOrdering -> -$ByteOrdering]}, {numofattr}]];
   ];
  If[numofattr == 0,
   {name, numRec, taglist, reflist, class, version},
   {name, numRec, taglist, reflist, class, version, numofattr, 
    attrindexlist}]]

(*HDF4TagList::usage="HDF4TagList list some important hdf tags.";*)

HDF4TagList = {{"DFTAG_VERSION", "Library Version Number", 
    "4 bytes+string", 30, 
    "Specifies the latest version of the HDF library used to write to \
the file."},
   {"DFTAG_FD", "File Descriptor Text", 101, 
    "Points to a block of text describing the overall file contents. \
It is intended to be usersupplied comments about the file."}, \
{"DFTAG_NT", "Number Type", "4 bytes", 106, 
    "Used by any other element in the file to specifically indicate \
what a numeric value looks like."},
   {"DFTAG_SDD", "SDS Dimension Record", "n bytes", 701, 
    "Defines the rank and dimensions of the array the corresponding \
SD refers to."},
   {"DFTAG_SD", "Scientific Data", "Real Number", 702, 
    "Points to scientific data."},
   {"DFTAG_NDG", "Numeric Data Group", "n*4 bytes", 720, 
    "Lists the Data Identifiers (tag/reference number pairs) that \
describe a scientific data set.Supersedes DFTAG_SDG."},
   {"DFTAG_VG", "Vgroup", "14+n bytes", 1965, 
    "Provides a general-purpose grouping structure."}, {"DFTAG_VH", 
    "Vdata Description", "22+n bytes", 1962, 
    "Provides information necessary to process a DFTAG_VS."}, \
{"DFTAG_VS", "Vdata", "n bytes", 1963, 
    "Contains a block a data that is to be interpreted according to \
the information in thecorresponding DFTAG_VH."}};

(*extractVdata::usage = 
  "extractVdata[stream] extracts all Vdata sets from a stream.";*)
extractVdata[stream_] := Module[{refs},
  refs = listVdataSets[stream];
  (getDFTAGVS[stream, #] /. {a__String} :> StringJoin@a) & /@ refs]

getVdataName[stream_, ref_] := getDFTAGVH[stream, ref][[1]]
getVdataFieldName[stream_, ref_] := getDFTAGVH[stream, ref][[7]]
getVdataDimensions[stream_, ref_] := getDFTAGVH[stream, ref][[{2, 4}]]
getVdataNumberOfRecords[stream_, ref_] := getDFTAGVH[stream, ref][[2]]
getVdataNumberOfFields[stream_, ref_] := getDFTAGVH[stream, ref][[4]]

(*findVdataName::usage = 
  "findVdataName[stream,VdataName] returns the reference number of \
the Vdata set with the name VdataName. If the name can not be found \
$Failed is returned.\n
  findVdataName[stream,Reference number] returns the name of the \
Vdata set with the corresponding Reference number.";*)
findVdataName[stream_, name_String] := Module[{dd, vdatas, names, res},
  dd = readDataDescriptor[stream];
  vdatas = Cases[dd, {1962, ___}];
  names = getVdataName[stream, #] & /@ vdatas[[All, 2]];
  res = Flatten[Cases[Transpose[{vdatas, names}], {{__}, name}], 1];
  If[res != {}, res[[1, 2]], $Failed]]
findVdataName[stream_, ref_Integer] := getVdataName[stream, ref]

(*findVdataFieldName::usage = 
  "findVdataFieldName[stream,VdataFieldName] returns the reference \
number and column positions of the Vdata set containing fields with \
the fields name VdataFieldName. If the name can not be found $Failed \
is returned.\n
  findVdataFieldName[stream,Reference number] returns the Vdata field \
names of the Vdata set with the corresponding Reference number.";*)
findVdataFieldName[stream_, name_String] := 
 Module[{dd, vdatas, names, res, pos},
  dd = readDataDescriptor[stream];
  vdatas = Cases[dd, {1962, ___}];
  names = getVdataFieldName[stream, #] & /@ vdatas[[All, 2]];
  pos = Position[names, name];
  res = Transpose[{vdatas[[pos[[All, 1]]]][[All, 2]], pos[[All, 2]]}];
  If[res != {}, res, $Failed]]
findVdataFieldName[stream_, ref_Integer] := 
 getVdataFieldName[stream, ref]

getVdataOffset[stream_, ref_] := Module[{dd, res},
  dd = readDataDescriptor[stream];
  res = Flatten[Cases[dd, {1963, ref, ___}]];
  If[res != {}, res[[3]], $Failed]]
getVdataLength[stream_, ref_] := Module[{dd, res},
  dd = readDataDescriptor[stream];
  res = Flatten[Cases[dd, {1963, ref, ___}]];
  If[res != {}, res[[4]], $Failed]]

HDF4ContainsVsetQ[stream_] := Module[{dd},
  dd = readDataDescriptor[stream];
  MemberQ[dd[[All, 1]] // Union, 1962 | 1963 | 1965]]
HDF4ContainsSDSQ[stream_] := Module[{dd},
  dd = readDataDescriptor[stream];
  MemberQ[dd[[All, 1]] // Union, 
   MemberQ[dd[[All, 1]] // Union, Alternatives @@ Range[700, 732]]]]
HDF4ContainsUtilityQ[stream_] := Module[{dd},
  dd = readDataDescriptor[stream];
  MemberQ[dd[[All, 1]] // Union, 
   MemberQ[dd[[All, 1]] // Union, Alternatives @@ Range[1, 110]]]]

flattenColumn[array_, col_Integer, lev_: 1] := Module[{arr},
  arr = array;
  arr[[All, col]] = Flatten[arr[[All, col]], lev];
  arr]

(*listVdataSets::usage = 
  "listVdataSets[stream] list the reference numbers of all contained \
Vdata sets with a
  DFTAG_VH (tag 1962) and DFTAG_VS (tag 1963) filed.";*)
listVdataSets[stream_] := Module[{dd, vh, vs, vhref, vsref},
  dd = readDataDescriptor[stream];
  vh = Cases[dd, {1962, ___}];
  vhref = vh[[All, 2]];
  vs = Cases[dd, {1963, ___}];
  vsref = vs[[All, 2]];
  Which[
   vhref == vsref, vhref,
   True, Intersection[vhref, vsref]]]

End[]

EndPackage[]

