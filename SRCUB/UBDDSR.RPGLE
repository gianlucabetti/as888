000100100811       //==============================================================
000200100811       //?UBDDSR - Visualizzazione tracciato record file               ?
000300100811       //==============================================================
000400100811
000500100811       //--------------------------------------------------------------
000600100811       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700100811       //--------------------------------------------------------------
000800100811
000900110126     /*PRM dftactgrp(*no) actgrp(*caller)
001000100811     /*PRM dbgview(*source) tgtrls(*current)
001100110126     /*CMD dspfd file(FNLV55DS) type(*accpth) +
001200110126     /*CMD       output(*outfile) outfile(QTEMP/QAFDACCP)
001300110126     /*END dltf file(QTEMP/QAFDACCP)
001400100811     /*END
001500100811
001600100811       //--------------------------------------------------------------
001700100811       //?Specifiche di controllo.                                     ?
001800100811       //--------------------------------------------------------------
001900100811
002000100811     h decedit('0,') datedit(*dmy/) debug(*yes) option(*nodebugio)
002100100811
002200100811       //--------------------------------------------------------------
002300100811       //?Dichiarazione file.                                          ?
002400100811       //--------------------------------------------------------------
002500100811
002600110126       // -?Descrizione archivio con i dati del file (*cmd DSPFD)?
002700100811     fQAFDACCP  if   e             disk    usropn
002800110126     f                                     extfile(wLibFile)
002900100811
003000110126       // -?Video?
003100100811     fUBDDSD    cf   e             workstn
003200100811     f                                     sfile(DDS_S1 : S1nrr)
003300100811     f                                     sfile(DDS_S2 : S1nrr)
003400100811     f                                     indds(IndDspF)
003500100811     f                                     infds(InfDspF)
003600100811
003700110126       // -?Stampa?
003800100811     fPrintDDS  o    f  132        printer
003900100811     f                                     infds(InfPrtF)
004000100811     f                                     oflind(*inOA)
004100100811     f                                     usropn
004200100811
004300100811       //--------------------------------------------------------------
004400100811       //?Definizione costanti.                                        ?
004500100811       //--------------------------------------------------------------
004600100811
004700100811       // -?Numero record visualizzati nel subfile S1?
004800100811     d c_SflPag        c                   const(14)
004900100811     d c_Copyright     c                   const('© copyright by +
005000100811     d                                            Gruppo PRO & +
005100100811     d                                            SETRAS Srl')
005200100811
005300100811       // -?Comando di verifica esistenza del file?
005400100811     d c_CmdCHKOBJ_a   c                   const('CHKOBJ obj(')
005500100811     d c_CmdCHKOBJ_b   c                   const(') +
005600100811     d                                       objtype(*file)')
005700100811
005800100811       // -?Comando di reperimento descrizione del file e?
005900100811       //             ?visualizzazione suoi dati a video?
006000100811     d c_CmdDSPFD_a    c                   const('DSPFD file(')
006100100811     d c_CmdDSPFD_b    c                   const(') +
006200110822     d                                       type(*accpth)')
006300110822     d c_CmdDSPFD_b2   c                   const(' +
006400100811     d                                       output(*outfile) +
006500100811     d                                       outfile(QTEMP/QAFDACCP)')
006600100811     d c_CmdDSPFD_c    c                   const(') +
006700100811     d                                       type(*select)')
006800100811
006900100811       // -?Comando di visualizzazione dati del file?
007000100811     d*// c_cmdDSPPFM_a   c                   const('DSPPFM ??file(')
007100100811     d*// c_cmdDSPPFM_b   c                   const(') +
007200100811     d*//                                       ??mbr(*n) +
007300100811     d*//                                       ??fromrcd(*n)')
007400100811     d c_cmdDSPDBF_a   c                   const('DSPDBF file(')
007500100811     d c_cmdDSPDBF_b   c                   const(') +
007600100811     d                                       ??mbr(*n) +
007700100811     d                                       ??rcdslt(*n) +
007800100811     d                                       ??output(*n)')
007900100811
008000100811       // -?Comando di visualizzazione relazioni del file?
008100100811     d c_cmdDSPDBR_a   c                   const('DSPDBR file(')
008200100811     d c_cmdDSPDBR_b   c                   const(') +
008300100811     d                                       output(*)')
008400100811
008500100811       // -?Comando di sostituzione file di stampa?
008600100811     d c_cmdOVRPRTF    c                   const('OVRPRTF +
008700100811     d                                       file(PrintDDS) +
008800100811     d                                       tofile(QSYSPRT) +
008900110215     d                                       pagesize(66 132 *rowcol) +
009000110215     d                                       lpi(6) cpi(10) +
009100100811     d                                       usrdta(''Print DDS'') +
009200100811     d                                       splfname(')
009300100811
009400100811       // -?Posizione di default per il cursore?
009500100811     d c_H1riga_dft    c                   const(05)
009600100811     d c_H1colo_dft_x_Fld...
009700100920     d                 c                   const(16)
009800100811     d c_H1colo_dft_x_Str...
009900100920     d                 c                   const(18)
010000100811     d c_PosCurFile_row...
010100100811     d                 c                   const(03)
010200100811     d c_PosCurFile_col_Libr...
010300100811     d                 c                   const(02)
010400100811     d c_PosCurFile_col_File...
010500100811     d                 c                   const(15)
010600100811
010700100811       // -?Tasti funzionali a video?
010800100811     d c_F01           c                   const(x'31')
010900100811     d c_F02           c                   const(x'32')
011000100811     d c_F03           c                   const(x'33')
011100100811     d c_F04           c                   const(x'34')
011200100811     d c_F05           c                   const(x'35')
011300100811     d c_F06           c                   const(x'36')
011400100811     d c_F07           c                   const(x'37')
011500100811     d c_F08           c                   const(x'38')
011600100811     d c_F09           c                   const(x'39')
011700100811     d c_F10           c                   const(x'3A')
011800100811     d c_F11           c                   const(x'3B')
011900100811     d c_F12           c                   const(x'3C')
012000100811     d c_F13           c                   const(x'B1')
012100100811     d c_F14           c                   const(x'B2')
012200100811     d c_F15           c                   const(x'B3')
012300100811     d c_F16           c                   const(x'B4')
012400100811     d c_F17           c                   const(x'B5')
012500100811     d c_F18           c                   const(x'B6')
012600100811     d c_F19           c                   const(x'B7')
012700100811     d c_F20           c                   const(x'B8')
012800100811     d c_F21           c                   const(x'B9')
012900100811     d c_F22           c                   const(x'BA')
013000100811     d c_F23           c                   const(x'BB')
013100100811     d c_F24           c                   const(x'BC')
013200100811     d c_Clear         c                   const(x'BD')
013300100811     d c_Enter         c                   const(x'F1')
013400100811     d c_Help          c                   const(x'F3')
013500100811     d c_RollDown      c                   const(x'F4')
013600100811     d c_RollUp        c                   const(x'F5')
013700100811     d c_Print         c                   const(x'F6')
013800100811     d c_RcdBackSpace  c                   const(x'F8')
013900100811     d c_AutoEnter     c                   const(x'3F')
014000100811
014100100811       // -?Attributi di visualizzazione?
014200100811     d c_X23           c                   const(X'23')
014300100811
014400100811       //--------------------------------------------------------------
014500100811       //?Definizione schiere.                                         ?
014600100811       //--------------------------------------------------------------
014700100811
014800100811       // -?Messaggi di errore?
014900100811     d $Msg            s             78    dim( 9) ctdata perrcd(1)
015000100811
015100100811       // -?Campi chiave del file: nomi?
015200101029     d sch_KFNA        s             10    dim(99) inz                          Key field name
015300100811       // -?Campi chiave del file: numero sequenza?
015400101029     d sch_KFNU        s              3  0 dim(99) inz                          Key field number
015500101029       // -?Campi chiave del file: sequenza (A=Ascend, D=Descend)?
015600101029     d sch_KFSQ        s              1    dim(99) inz                          Key field sequence
015700100811
015800100811       //--------------------------------------------------------------
015900100811       //?Definizione aree dati.                                       ?
016000100811       //--------------------------------------------------------------
016100100811
016200100811
016300100811       //--------------------------------------------------------------
016400100811       //?Definizione strutture dati.                                  ?
016500100811       //--------------------------------------------------------------
016600100811
016700100811       // -?Status ds?
016800100811     d Status         sds
016900100811     d  SDSpgm           *proc
017000100811
017100100811       // -?InfDS del DspF?
017200100811     d InfDspF         ds                  qualified
017300100811     d   dsp_aid             369    369a                                        AID byte
017400100811     d   dsp_aidInt          369    369u 0
017500100811         // -?Controllo subfile?
017600100811     d   cursor_Pos          370    371i 0
017700100811     d     cursor_Row        370    370i 0
017800100811     d     cursor_Col        371    371i 0
017900100811     d   sf_rrn              376    377i 0                                      Subfile rrn
018000100811     d   min_nrr             378    379i 0                                      Subfile min rrn
018100100811     d   num_rcds            380    381i 0                                      Subfile num rcds
018200100811     d   act_row             382    382                                         Cursor position
018300100811     d   act_col             383    383                                         Cursor position
018400100811
018500100811       // -?Posizione cursore?
018600101029     d Cursor          ds                  inz
018700100811     d   RowInt                1      2b 0 inz
018800101029     d   RowChr                2      2    inz
018900100811     d   ColInt                3      4b 0 inz
019000101029     d   ColChr                4      4    inz
019100100811
019200100811       // -?InfDS del PrtF?
019300100811     d InfPrtF         ds                  qualified
019400100811         // -?Controllo overflow?
019500100811     d   OvrFlw              367    368b 0
019600100811
019700100811       // -?Indicatori su DspF?
019800101029     d IndDspF         ds
019900100811         // -?Abilitazione tasti funzionali?
020000110822     d   GestioneF09                   n   overlay(IndDspF : 09)
020100100811     d   GesVistaF11                   n   overlay(IndDspF : 11)
020200110822     d   GestioneF14                   n   overlay(IndDspF : 14)
020300100811     d   GestioneF22                   n   overlay(IndDspF : 22)
020400100811         // -?Gestione del subfile?
020500100811     d   SflDsp_N                      n   overlay(IndDspF : 30)
020600100811     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
020700100811     d   SflNxtChg                     n   overlay(IndDspF : 32)
020800100811     d   SflEnd                        n   overlay(IndDspF : 33)
020900100811         // -?Condizionamento attributi di visualizzazione?
021000100811     d   FileIntest                    n   overlay(IndDspF : 40)
021100100811     d   FieldAlfa                     n   overlay(IndDspF : 41)
021200100811     d   FieldFound                    n   overlay(IndDspF : 42)
021300101029     d   OrdDescend                    n   overlay(IndDspF : 43)
021400100811     d   ErrFile                       n   overlay(IndDspF : 51)
021500100811     d   ErrField                      n   overlay(IndDspF : 52)
021600100811         // -?Emissione messaggio di errore?
021700100811     d   ErrMessage                    n   overlay(IndDspF : 98)
021800100811         //   -?Riemissione videata?
021900100811     d   ErrGenerico                   n   overlay(IndDspF : 99)
022000100811
022100100811       // -?Composizione del nome del file e della relativa libreria?
022200100811     d ds_FileLib      ds                  inz
022300100811     d   WHfile                            inz
022400100811     d   WHlib                             inz
022500100811
022600100811       // -?Definizione campi per acquisizione informazioni del file?
022700100811     d ds_Info_1       ds                  inz   qualified
022800100811     d   UsrSpc                      20    inz('DDSINFO   QTEMP     ')
022900100811     d   OutFmt                       8    inz
023000100811     d   FilLib                      20    inz
023100100811     d     FilName            29     38    inz
023200100811     d     LibName            39     48    inz
023300100811     d   RcdFMI                      10    inz
023400100811     d   OvrRID                       1    inz(*zero)
023500100811
023600100811     d Save_ds_Info_1  s                   inz   like(ds_Info_1)
023700100811
023800100811       // -?Definizione campi per acquisizione informazioni RCDFMT?
023900100811     d ds_Info_2       ds                  inz   qualified
024000100811     d   UsrSpc                1     20    inz('FMTINFO   QTEMP     ')
024100100811     d   OutFmt                       8    inz
024200100811     d   FilLib                      20    inz
024300100811     d     FilName            29     38    inz
024400100811     d     LibName            39     48    inz
024500100811     d   RcdFMI                      10    inz
024600100811     d   OvrRID                       1    inz(*zero)
024700100811
024800100811     d Save_ds_Info_2  s                   inz   like(ds_Info_2)
024900100811
025000100811       // -?Definizione campi per posizionamento API?
025100100811     d ds_Pos_1        ds                  inz   qualified
025200100811     d   LenDta                      10i 0 inz
025300100811     d   StrPos                      10i 0 inz
025400100811     d ds_Pos_2        ds                  likeds(ds_Pos_1)
025500100811     d                                     inz
025600100811
025700100811       // -?Dati di TESTATA?
025800100811     d ds_RcvVar_Txt   ds                  inz   qualified
025900100811     d   OffSet                1      4b 0 inz
026000100811     d   HdrSiz                5      8b 0 inz
026100100811     d*//NoEntr                9     12b 0 inz
026200100811     d*//LstSiz               13     16b 0 inz
026300100811       // -?Dati di lista FFD?
026400100811     d ds_RcvVar_FFD   ds                  inz   qualified
026500100811     d   OffSet                1      4b 0 inz
026600100811     d*//HdrSiz                5      8b 0 inz
026700100811     d   NoEntr                9     12b 0 inz
026800100811     d   LstSiz               13     16b 0 inz
026900100811       // -?Dati di lista RCDFMT?
027000100811     d ds_RcvVar_RCF   ds                  inz   qualified
027100100811     d   OffSet                1      4b 0 inz
027200100811     d*//HdrSiz                5      8b 0 inz
027300100811     d   NoEntr                9     12b 0 inz
027400100811     d   LstSiz               13     16b 0 inz
027500100811
027600100811       // -?Definizione dati di testata?
027700100811     d ds_Header       ds                  inz   qualified
027800100811     d   FilLib                      20    inz
027900100811     d     FilName             1     10    inz
028000100811     d     LibName            11     20    inz
028100100811     d   FilTyp                      10    inz
028200100811     d   RcdFmt                      10    inz
028300100811     d   RcdLen                       9b 0 inz
028400100811     d   RcdID                       13    inz
028500100811     d   Text                        50    inz
028600100811
028700100811       // -?Definizione lista dati informazione campi?
028800101029     d ds_List         ds                  inz
028900101029     d   V1FldNam                    10    inz
029000101029     d   V1DtaTyp                     1    inz
029100101029     d   Usage                        1    inz
029200101029     d   OutBuf                       9b 0 inz
029300101029     d   InpBuf                       9b 0 inz
029400101029     d   FldLen                       9b 0 inz
029500101029     d   Digits                       9b 0 inz
029600101029     d   DecPos                       9b 0 inz
029700101029     d   H1text                      50    inz
029800101029     d     V1des              33     78    inz
029900101029     d     Filler             79     82    inz
030000101029     d   H1EdtCde                     2    inz
030100101029     d   EdtLen                       9b 0 inz
030200101029     d   EdtWrd                      64    inz
030300101029     d   V2ColHd1                    20    inz
030400101029     d   V2ColHd2                    20    inz
030500101029     d   V2ColHd3                    20    inz
030600100811
030700100811       // -?Definizione lista dati informazione campi?
030800100811     d ds_FoundString  ds                  inz   qualified
030900100811     d  fs_Row                 1      2  0 inz
031000100811     d  fs_col                 3      4  0 inz
031100100811
031200100811       //--------------------------------------------------------------
031300100811       //?Definizione variabili globali.                               ?
031400100811       //--------------------------------------------------------------
031500100811
031600100811       // -?Parametri ricevuti?
031700100811     d pi_FilLib       s             20
031800100811     d pi_FldNap       s             10
031900100811
032000100811       // -?Flags booleani?
032100100811     d $Fine           s               n   inz
032200100811     d $InzS1          s               n   inz(*on)
032300100811     d $FirstFmt       s               n   inz(*on)
032400100811     d $FldFound       s               n   inz
032500100811     d $StrFound       s               n   inz
032600100811
032700100811       // -?Indici di schiera?
032800100811     d xx              s              3i 0 inz
032900100811
033000100811       // -?Variabili per la gestione del video?
033100100811     d $Video          s              2    inz('S1')
033200100811     d S1nrr           s                   like(C1RcdNbr) inz
033300110126
033400110126       // -?Nome esteso Libreria/File del file QAFDACCP da elaborare?
033500110126     d wLibFile        s             21a   inz('QTEMP/QAFDACCP')
033600100811
033700100811       // -?Campi di comodo?
033800100811     d SaveFLDE        s                   like(WHflde)   inz
033900100811     d SaveSflRrn      s                   like(S1nrr)    inz
034000100811     d w_MaxSflRrn     s                   like(S1nrr)    inz
034100100811     d w_LastSflRrn    s                   like(S1nrr)    inz
034200100811     d w_SaveRcdFms    s                   like(H1RcdFms) inz
034300100811     d w_Count         s             10i 0 inz
034400100811     d w_StrPos        s             10i 0 inz
034500100811
034600100811       // -?Campi di stampa?
034700100811     d s_Time          s              6  0 inz
034800100811     d s_ObjTxt        s             50    inz
034900100811     d s_WHname        s            120    inz
035000100811
035100100811       //--------------------------------------------------------------
035200100811       //?Definizione prototipi procedure.                             ?
035300100811       //--------------------------------------------------------------
035400100811
035500100811       // -?Parametri API QCAPCMD (Process Commands)?
035600100811     d Qcmd            s           2048    inz   varying
035700100811      /copy qSysInc/qRpgleSrc,QCAPCMD
035800100811      /copy gaitrasrc/srcProtoPR,QCAPCMD
035900100811
036000100811       // -?Parametri API QCMDEXC (Execute Commands)?
036100100811      /copy gaitrasrc/srcProtoPR,QCMDEXC
036200100811
036300100811       // -?Parametri gestione errori API.?
036400100811      /copy qsysinc/qrpglesrc,QUSEC
036500100811
036600100811       // -?Parametri API QUSROBJD (Retrieve Object Description)?
036700100811      /copy qSysInc/qRpgleSrc,QUSROBJD
036800100811      /copy GaiTraSrc/srcProtoPr,QUSROBJD
036900100811
037000100811       // -?Parametri API QUSRTVUS (Retrieve User Space)?
037100100811     d QUSRTVUS        pr                  extpgm('QUSRTVUS')
037200100811     d   QualifiedUserSpaceName...
037300100811     d                               20    const
037400100811     d   StartingPosition...
037500100811     d                               10i 0 const
037600100811     d   LengthOfData...
037700100811     d                               10i 0 const
037800100811     d   ReceiverVariable...
037900100811     d                              512a   options(*varsize)
038000100811     d   ErrorCode...
038100100811     d                                     like(QUSEC)
038200100811     d                                     options(*varsize:
038300100811     d                                             *nopass : *omit)
038400100811
038500100811       // -?Parametri API QUSLRCD (List Record Formats)?
038600100811     d QUSLRCD         pr                  extpgm('QUSLRCD')
038700100811     d   QualifiedUserSpaceName...
038800100811     d                               20    const
038900100811     d   FormatName...
039000100811     d                                8    const
039100100811     d   QualifiedFileName...
039200100811     d                               20    const
039300100811     d   OverrideProcessing...
039400100811     d                                1    const
039500100811     d   ErrorCode...
039600100811     d                                     like(QUSEC)
039700100811     d                                     options(*varsize :
039800100811     d                                             *nopass : *omit)
039900100811
040000100811       // -?Parametri API QUSLFLD (List Fields)?
040100100811     d QUSLFLD         pr                  extpgm('QUSLFLD')
040200100811     d   QualifiedUserSpaceName...
040300100811     d                               20    const
040400100811     d   FormatName...
040500100811     d                                8    const
040600100811     d   QualifiedFileName...
040700100811     d                               20    const
040800100811     d   RecordFormatName...
040900100811     d                               10    const
041000100811     d   OverrideProcessing...
041100100811     d                                1    const
041200100811     d   ErrorCode...
041300100811     d                                     like(QUSEC)
041400100811     d                                     options(*varsize :
041500100811     d                                             *nopass : *omit)
041600100811
041700100811       // -?Parametri API QUSCMDLN (Display Command Line Window)?
041800100811     d QUSCMDLN        pr                  extpgm('QUSCMDLN')
041900100811
042000100811       // -?Parametri API QDCXLATE (Convert Data)?
042100100811     d QDCXLATE        pr                  extpgm('QDCXLATE')
042200100811     d   LengthOfDataBeingConverted...
042300100811     d                                5  0 const
042400100811     d   ConversionData...
042500100811     d                            32767a   options(*varsize)
042600100811     d   SBCSconversionTableName...
042700100811     d                               10    const
042800100811     d   SBCSconversionTableLibraryName...
042900100811     d                               10    const
043000110126
043100110126       //--------------------------------------------------------------
043200110126       //?Indicatori utilizzati.                                       ?
043300110126       //--------------------------------------------------------------
043400110126
043500110126       //?*in39?- NON stampa "ordinamento chiavi di accesso"
043600100811
043700100811      /TITLE DDS_R - Flusso programma
043800100811
043900100811       //--------------------------------------------------------------
044000100811       //?M A I N - L I N E                                            ?
044100100811       //--------------------------------------------------------------
044200100811
044300100811     c     *Entry        plist
044400100811     c                   parm                    pi_FilLib
044500100811     c                   parm                    pi_FldNap
044600100811
044700100811      /free
044800100811
044900100811       // -?Operazioni iniziali?
045000100811       exsr sr_RoutInz;
045100100811
045200100811       // -?Gestione videate?
045300100811       DoW  $Fine = *off;
045400100811
045500100811         select;
045600100811
045700100811           when  $Video = 'S1';
045800100811             exsr  sr_GesS1;
045900100811
046000100811           when  $Video = 'S2';
046100100811             exsr  sr_GesS1;
046200100811
046300100811           other;
046400100811             $Fine = *on;
046500100811
046600100811         endsl;
046700100811
046800100811       EndDo;
046900100811
047000100811       // -?Operazioni finali?
047100100811       exsr sr_RoutEnd;
047200100811
047300100811       //--------------------------------------------------------------
047400100811       //?Operazioni iniziali.                                         ?
047500100811       //--------------------------------------------------------------
047600100811       BEGSR  sr_RoutInz;
047700100811
047800100811         *inLR = *on;
047900100811
048000100811         // -?Impostazione nome programma a video?
048100100811         //V1Tpgm = SDSpgm;
048200100811
048300100811         // -?Reperimento orario iniziale?
048400100811         s_Time = %dec( %time() );
048500100811
048600100811       ENDSR;
048700100811
048800100811       //--------------------------------------------------------------
048900100811       //?Gestione subfile S1.                                         ?
049000100811       //--------------------------------------------------------------
049100100811       BEGSR  sr_GesS1;
049200100811
049300100811         // -?Inizializzazione videata?
049400100811         if  $InzS1 = *on;
049500100811           exsr  sr_InzS1;
049600100811           $InzS1 = *off;
049700100811         endif;
049800100811
049900100811         // -?Emissione videata?
050000100811         write  DDS_T1;
050100100811         write  DDS_T2;
050200100811         write  DDS_P1;
050300100811         if  NOT  GesVistaF11;
050400100811           if  ErrMessage;
050500100811             write  DDS_T2;
050600100811             write  DDS_C1;
050700100811           endif;
050800100811           exfmt  DDS_C1;
050900100811         else;
051000100811           if  ErrMessage;
051100100811             write  DDS_T2;
051200100811             write  DDS_C2;
051300100811           endif;
051400100811           exfmt  DDS_C2;
051500100811         endif;
051600100811
051700100811         clear  V1msg;
051800100811         %subst(IndDspF : 50) = *off;
051900100811
052000100811         // -?Posiziono il cursore dove era?
052100100811         H1riga = InfDspF.Cursor_Row;
052200100811         H1colo = InfDspF.Cursor_Col;
052300100811
052400100811         select;
052500100811           when  SaveSflRrn > *zero   and   WHflde <> *blank;
052600100811             C1RcdNbr = SaveSflRrn;
052700100811           when  InfDspF.min_Nrr > *zero;
052800100811             C1RcdNbr = InfDspF.min_nrr;
052900100811         endsl;
053000100811
053100100811         if  InfDspf.dsp_aid = c_F16   or
053200100811             InfDspf.dsp_aid = c_F17   or
053300100811             InfDspf.dsp_aid = c_Enter;
053400100811           read  DDS_T2;
053500100811         endif;
053600100811
053700100811         SELECT;
053800100811
053900100811           // -?F3 = Fine?
054000100811           WHEN  InfDspf.dsp_aid = c_F03;
054100100811             $Fine = *on;
054200100811
054300100811           // -?F8 = Stampa?
054400100811           WHEN  InfDspf.dsp_aid = c_F08;
054500100811             exsr  sr_PrintD;
054600100811
054700100811           // -?F9 = Select/Omit?
054800100811           WHEN  InfDspf.dsp_aid = c_F09;
054900100811             exsr  sr_Omit;
055000100811
055100100811           // -?F11 = Vista 1 / Vista 2?
055200100811           WHEN  InfDspf.dsp_aid = c_F11;
055300100811             GesVistaF11   = (GesVistaF11 = *off);
055400100811             if  GesVistaF11;
055500100811               GestioneF22 = *off;
055600101112               if  WHflde = *blank;
055700101112                 H1riga = c_H1riga_dft;
055800101112                 H1colo = c_H1colo_dft_x_Fld;
055900101112               endif;
056000100811             endif;
056100100811
056200100811           // -?F12 = Ritorno?
056300100811           WHEN  InfDspf.dsp_aid = c_F12;
056400100811             $Fine = *on;
056500100811
056600110822           // -?F13 = Visualizzazione LF?
056700110822           WHEN  InfDspf.dsp_aid = c_F13  and  ds_Header.FilTyp = 'PF';
056800100811             Qcmd = c_CmdDSPDBR_a
056900100811                  + %trimr(WHlib) + '/' + %trimr(WHfile)
057000100811                  + c_CmdDSPDBR_b;
057100100811             exsr  sr_ExecCmd;
057200110822           // -?F13 = Visualizzazione PF?
057300110822           WHEN  InfDspf.dsp_aid = c_F13  and  ds_Header.FilTyp = 'LF';
057400110822             Qcmd = c_CmdDSPFD_a
057500110822                  + %trimr(WHlib) + '/' + %trimr(WHfile)
057600110822                  + c_CmdDSPFD_b;
057700110822             exsr  sr_ExecCmd;
057800100811
057900100811           // -?F14 = Visualizzazione dati?
058000100811           WHEN  InfDspf.dsp_aid = c_F14;
058100100811             //Qcmd = c_CmdDSPPFM_a
058200100811             //     + %trimr(WHlib) + '/' + %trimr(WHfile)
058300100811             //     + c_CmdDSPPFM_b;
058400100811             Qcmd = c_CmdDSPDBF_a
058500100811                  + %trimr(WHlib) + '/' + %trimr(WHfile)
058600100811                  + c_CmdDSPDBF_b;
058700100811             //exsr  sr_ExecCmd;
058800100811             callp(e)  ExecuteCommand ( Qcmd : %size(Qcmd) );
058900100811
059000100811           // -?F16 = Ricerca stringa avanti?
059100100811           WHEN  InfDspf.dsp_aid = c_F16;
059200100811             exsr  sr_ScanString;
059300100811
059400100811           // -?F17 = Ricerca stringa indietro?
059500100811           WHEN  InfDspf.dsp_aid = c_F17;
059600100811             exsr  sr_ScanString;
059700100811
059800100811           // -?F21 = Riga comandi?
059900100811           WHEN  InfDspF.dsp_aid = c_F21;
060000100811             QUSCMDLN ();
060100100811
060200100811           // -?F22 = Ricerca stringa <=> Ricerca campo?
060300100811           WHEN  InfDspf.dsp_aid = c_F22;
060400100811             H1riga = c_H1riga_dft;
060500100811             if  GestioneF22;
060600100811               GestioneF22 = *off;
060700100811               H1colo = c_H1colo_dft_x_Fld;
060800100811               clear  WHflde;
060900131120               clear  SaveFLDE;
061000131120               clear  $FldFound;
061100100811             else;
061200100811               GestioneF22 = *on;
061300100811               H1colo = c_H1colo_dft_x_Str;
061400100811               clear  V1string;
061500100811               exsr  sr_ResetSfl;
061600100811             endif;
061700100811
061800100811           // -?Invio?
061900100811           OTHER;
062000100811             exsr  sr_CtrlT2;
062100100811             if  ErrGenerico = *on   or   $InzS1 = *on;
062200100811               leavesr;
062300100811             endif;
062400100811             exsr  sr_GesT2;
062500100811
062600100811         ENDSL;
062700100811
062800100811       ENDSR;
062900100811
063000100811       //--------------------------------------------------------------
063100100811       //?Inizializzazione subfile S1 e S2.                            ?
063200100811       //--------------------------------------------------------------
063300100811       BEGSR  sr_InzS1;
063400100811
063500100811         // -?Pulizia subfile?
063600100811         clear  DDS_T2;
063700100811         clear  DDS_C1;
063800100811         clear  DDS_C2;
063900100811         SflDsp_N    = *on;
064000100811         SflDspCtl_N = *on;
064100100811         write  DDS_C1;
064200100811         write  DDS_C2;
064300100811         SflDspCtl_N = *off;
064400100811
064500100811         SflEnd      = *off;
064600100811         clear  S1nrr;
064700100811         clear  C1RcdNbr;
064800100811         clear  C1CsrRrn;
064900100811         ErrMessage  = *off;
065000100811         ErrGenerico = *off;
065100100811
065200100811         // -?Spegnimento degli indicatori di errore?
065300100811         %subst(IndDspF : 50) = *off;
065400100811
065500100811         // -?Impostazione nome archivio (ricevuto) a video?
065600100811         ds_FileLib = pi_FilLib;
065700100811         ds_Info_1.FilLib = pi_FilLib;
065800100811         ds_Info_2.FilLib = pi_FilLib;
065900100811
066000100811         // -?Ricevuto campo da ricercare?
066100100811         if  pi_FldNap = '*NONE';
066200100811           clear  WHflde;
066300100811         else;
066400100811           WHflde = pi_FldNap;
066500100811         endif;
066600100811
066700100811         // -?Pulizia del n° campi nel trk?
066800100811         clear  WHfldn;
066900100811
067000100811         // -?Pulizia del nome formato?
067100100811         clear  s_WHname;
067200100811
067300100811         // -?Retrieve file description?
067400100811         clear QUSD0200;
067500100811         clear QUSEC;
067600100811         Qusbprv = %size(Qusec);
067700100811         RetrieveObjectDescription( QUSD0200 :
067800100811                                    %len(QUSD0200) :
067900100811                                    'OBJD0200' :
068000100811                                    pi_FilLib :
068100100811                                    '*FILE' :
068200100811                                    qUsec );
068300100811         if  qUsei = *blank;
068400100811           s_ObjTxt = qUstd12;
068500100811         else;
068600100811           s_ObjTxt = *all'? ';
068700100811         endif;
068800100811
068900100811         // -?Fill the USRSPC whith record format names?
069000100811         ds_Info_2.OutFmt = 'RCDL0100';
069100100811         ds_Info_2.OvrRID = *zero;
069200100811         QUSLRCD ( ds_Info_2.UsrSpc :
069300100811                   ds_Info_2.OutFmt :
069400100811                   ds_Info_2.FilLib :
069500100811                   ds_Info_2.OvrRID :
069600100811                   Qusec );
069700100811
069800100811         // -?Find offset, size of each entry and the number of entries?
069900100811         //  ?Posizione 125-128 = Scostamento?
070000100811         //  ?          133-136 = Numero Totale di Voci?
070100100811         //  ?          137-140 = Lunghezza di ogni voce di elenco?
070200100811         ds_Pos_2.StrPos = 125;
070300100811         ds_Pos_2.LenDta = %size(ds_RcvVar_RCF);
070400100811         clear  ds_RcvVar_RCF;
070500100811         QUSRTVUS ( ds_Info_2.UsrSpc :
070600100811                    ds_Pos_2.StrPos :
070700100811                    ds_Pos_2.LenDta :
070800100811                    ds_RcvVar_RCF :
070900100811                    Qusec );
071000100811
071100100811         // -?Initialize start position, size of each entry and loop control?
071200100811         ds_Pos_2.StrPos = ds_RcvVar_RCF.OffSet + 1;
071300100811         ds_Pos_2.LenDta = ds_RcvVar_RCF.LstSiz;
071400100811         w_Count = 1;
071500100811
071600100811         // -?Loop until loop control greater than number of entries?
071700100811         DoW  w_Count <= ds_RcvVar_RCF.NoEntr;
071800100811
071900100811           // -?Retrieve record format names from the user space?
072000100811           QUSRTVUS ( ds_Info_2.UsrSpc :
072100100811                      ds_Pos_2.StrPos :
072200100811                      ds_Pos_2.LenDta :
072300100811                      ds_Info_2.RcdFMI :
072400100811                      Qusec );
072500100811
072600100811           // -?Fill the USRSPC with field file description?
072700100811           ds_Info_1.OutFmt = 'FLDL0100';
072800100811           ds_Info_1.OvrRID = *zero;
072900100811           ds_Info_1.RcdFMI = ds_Info_2.RcdFMI;
073000100811           QUSLFLD ( ds_Info_1.UsrSpc :
073100100811                     ds_Info_1.OutFmt :
073200100811                     ds_Info_1.FilLib :
073300100811                     ds_Info_1.RcdFMI :
073400100811                     ds_Info_1.OvrRID :
073500100811                     Qusec );
073600100811
073700100811           // -?Find offset and size of Header section?
073800100811           ds_Pos_1.StrPos = 117;
073900100811           ds_Pos_1.LenDta = %size(ds_RcvVar_Txt);
074000100811           clear  ds_RcvVar_Txt;
074100100811           QUSRTVUS ( ds_Info_1.UsrSpc :
074200100811                      ds_Pos_1.StrPos :
074300100811                      ds_Pos_1.LenDta :
074400100811                      ds_RcvVar_Txt :
074500100811                      Qusec );
074600100811
074700100811           // -?Initialize start position and size of heder section?
074800100811           ds_Pos_1.StrPos = ds_RcvVar_Txt.OffSet + 1;
074900100811           ds_Pos_1.LenDta = ds_RcvVar_Txt.HdrSiz;
075000100811
075100100811           // -? PERCHÈ C+--O RISULTANO "IMBANANATE" QUESTE 2 DS ?
075200100811           //  ? DOPO IL RICHIAMO DELLA PROSSIMA API QUSRTVUS ?? ?
075300100811           save_ds_Info_1 = ds_Info_1;
075400100811           save_ds_Info_2 = ds_Info_2;
075500100811
075600100811           // -?Retrieve header section?
075700100811           QUSRTVUS ( ds_Info_1.UsrSpc :
075800100811                      ds_Pos_1.StrPos :
075900100811                      ds_Pos_1.LenDta :
076000100811                      ds_Header :
076100100811                      Qusec );
076200100811
076300100811           // -? ... (VEDI SOPRA) ??? ?
076400100811           ds_Info_1 = Save_ds_Info_1;
076500100811           ds_Info_2 = Save_ds_Info_2;
076600100811
076700100811           // -?Retrive key fields?
076800100811           if  $FirstFmt;
076900100811             exsr  sr_RtvKey;
077000100811           endif;
077100100811
077200100811           // -?Find offset, size of each entry and the number of entries?
077300100811           ds_Pos_1.StrPos = 125;
077400100811           ds_Pos_1.LenDta = %size(ds_RcvVar_FFD);
077500100811           clear  ds_RcvVar_FFD;
077600100811           QUSRTVUS ( ds_Info_1.UsrSpc :
077700100811                      ds_Pos_1.StrPos :
077800100811                      ds_Pos_1.LenDta :
077900100811                      ds_RcvVar_FFD :
078000100811                      Qusec );
078100100811
078200100811           // -?Memo record format name?
078300100811           if  s_WHname = *blank;
078400100811             T1name   = %trimr(ds_Info_1.RcdFMI);
078500100811             s_WHname = %trimr(ds_Info_1.RcdFMI);
078600100811           else;
078700100811             T1name   = '(diversi) ';
078800100811             s_WHname = %trimr(s_WHname) + ', ' + ds_Info_1.RcdFMI;
078900100811           endif;
079000100811
079100100811           // -?Fill subfile with field file description information?
079200100811           exsr  sr_WriteS1;
079300100811
079400100811           // -?Set next entry start position?
079500100811           ds_Pos_2.StrPos += ds_RcvVar_RCF.LstSiz;
079600100811           w_Count += 1;
079700100811
079800100811         EndDo;
079900100811
080000100811         // -?Memo record format name?
080100100811         s_WHname = %trimr(s_WHname) + '.';
080200100811
080300100811         // -?Visualizzazione subfile?
080400100811         SflDsp_N = (S1nrr <= *zero);
080500100811         SflEnd   = *on;
080600100811         // -?Salvataggio ultimo rrn del subfile?
080700100811         w_LastSflRrn = S1nrr;
080800100811         // -?Riposizionamento subfile al 1° record?
080900100811         if S1nrr > *zero;
081000100811           S1nrr    = 1;
081100100811           C1RcdNbr = 1;
081200100811           C1CsrRrn = C1RcdNbr;
081300100811         else;
081400100811           clear C1RcdNbr;
081500100811         endif;
081600100811         // -?Posizionamento cursore su "Ricerca campo"?
081700100811         H1riga = c_H1riga_dft;
081800100811         H1colo = c_H1colo_dft_x_Fld;
081900100811
082000100811         // -?Impostazione indicatori per abilitazione tasti funzionali:?
082100110822         // ·?F09=Visualizzazione Select/Omit?
082200110822         GestioneF09 = (ds_Header.FilTyp = 'LF');
082300110822         // ·?F14=Visualizzazione dati?
082400110822         GestioneF14 = (ds_Header.FilTyp = 'PF'  or
082500110822                        ds_Header.FilTyp = 'LF');
082600100811
082700100811         // -?Impostazione messaggio iniziale?
082800100811         V1msg = c_Copyright;
082900100811         ErrMessage  = *on;
083000100811         ErrGenerico = *on;
083100100811
083200100811         // -?Impostazioni iniziali?
083300100811         WHrlen = ds_Header.RcdLen;
083400100811
083500100811         select;
083600100811
083700100811           // -?Richiesto posizionamento iniziale in ultima pagina?
083800100811           //  ?(dal chiamante)?
083900100811           when  WHflde = 'B'   or   WHflde = 'BOT';
084000100811             S1nrr = w_MaxSflRrn;
084100100811
084200100811           // -?Richiesto posizionamento iniziale in prima pagina?
084300100811           //  ?(dal chiamante)?
084400100811           when  WHflde = 'T'   or   WHflde = 'TOP';
084500100811             S1nrr = 1;
084600100811
084700100811           // -?Richiesta ricerca inziale campo (dal chiamante)?
084800100811           //  ?Scan field name if changed?
084900100811           when  WHflde <> SaveFLDE;
085000100811             exsr  sr_ScanField;
085100100811
085200100811         endsl;
085300100811
085400100811       ENDSR;
085500100811
085600100811      /TITLE SR_RTVKEY - Retrieve key fields and header's screen information
085700100811
085800100811       //--------------------------------------------------------------
085900100811       //?Retrieve key fields and header's screen information          ?
086000100811       //--------------------------------------------------------------
086100100811       BEGSR  sr_RtvKey;
086200100811
086300100811         // -?Reperimento informazioni relative al file di dati?
086400100811         IF  ds_Header.FilTyp = 'PF'   or
086500100811             ds_Header.FilTyp = 'LF';
086600100811
086700100811           Qcmd = c_CmdDSPFD_a
086800100811                + %trimr(WHlib) + '/' + %trimr(WHfile)
086900110822                + c_CmdDSPFD_b + c_CmdDSPFD_b2;
087000100811           exsr  sr_ExecCmd;
087100100811
087200100811           clear  sch_KFNA;
087300100811           clear  sch_KFNU;
087400101029           clear  sch_KFSQ;
087500100811           clear  xx;
087600100811
087700100811           open  QAFDACCP;
087800100811
087900100811           // -?Fill key field name and key field number arrays?
088000100811           read  QWHFDACP;
088100100811
088200100811           select;
088300100811             when  apUniq = 'Y';
088400100811               T1keyo = 'UNIQUE';
088500100811             when  apKeyO = 'L';
088600100811               T1keyo = 'LIFO  ';
088700100811             when  apKeyO = 'F';
088800100811               T1keyo = 'FIFO  ';
088900100811             when  apKeyO = 'C';
089000100811               T1keyo = 'FCFO  ';
089100100811             when  apKeyO = 'N';
089200100811               T1keyo = '*NONE ';
089300100811             other;
089400110126               //T1keyo = *all'-';
089500110126               //clear  T1keyo;
089600110126               T1keyo = apUniq + ' ' + apKeyO;
089700110126               *in39 = *on;
089800100811           endsl;
089900100811
090000100811           If  apNKYF <> *zero;
090100100811
090200100811             DoW  NOT %eof(QAFDACCP);
090300101029               xx += 1;
090400100811               sch_KFNA(xx) = apKeyF;
090500100811               sch_KFNU(xx) = apKeyN;
090600101029               sch_KFSQ(xx) = apKSeq;
090700100811               read  QWHFDACP;
090800100811             EndDo;
090900100811
091000100811           EndIf;
091100100811
091200100811           close  QAFDACCP;
091300100811
091400100811         ENDIF;
091500100811
091600100811         // -?Set header's screen data?
091700100811         T1ftyp = ds_Header.FilTyp;
091800100811         T1text = s_ObjTxt;
091900100811         WHfile = ds_Header.FilName;
092000100811         WHlib  = ds_Header.LibName;
092100100811         ds_Info_1.FilLib = ds_Header.FilLib;
092200100811
092300100811       ENDSR;
092400100811
092500100811      /TITLE SR_WRITES1 - Fill subfile
092600100811
092700100811       //--------------------------------------------------------------
092800100811       //?Fill subfile                                                 ?
092900100811       //--------------------------------------------------------------
093000100811       BEGSR  sr_WriteS1;
093100100811
093200101029         //?Write subfile with text and record format name?
093300100811
093400100811         // -?Impostazione indicatori per 1° record?
093500100811         FileIntest = *on;
093600100811         FieldAlfa  = *on;
093700101029         FieldFound = *off;
093800101029         OrdDescend = *off;
093900100811
094000100811         // -?Impostazione campi nel 1° record del subfile?
094100100811         Hind41   =  FieldAlfa;
094200100811         H1rcdfms = *all'-';
094300100811         H1text   = ds_Header.Text;
094400100811         V1fldnam = ds_Info_1.RcdFMI;
094500100811
094600100811         S1nrr += 1;
094700100811         write  DDS_S1;
094800100811         write  DDS_S2;
094900100811
095000100811         clear  DDS_S1;
095100100811         clear  DDS_S2;
095200100811
095300100811         FileIntest = *off;
095400100811
095500100811         clear  H1RcdFms;
095600100811
095700101029         //?Find offset, size of each entry and the number of entries?
095800100811
095900100811         ds_Pos_1.StrPos = ds_RcvVar_FFD.OffSet + 1;
096000100811         ds_Pos_1.LenDta = ds_RcvVar_FFD.LstSiz;
096100100811         w_MaxSflRrn = 1;
096200100811
096300100811         // -?Loop until loop control greater than number of entries?
096400100811         DOW  w_MaxSflRrn <= ds_RcvVar_FFD.NoEntr;
096500100811
096600100811           // -?Retrieve field file description from the user space?
096700100811           ds_Pos_1.LenDta = %size(ds_List);
096800100811           QUSRTVUS ( ds_Info_1.UsrSpc :
096900100811                      ds_Pos_1.StrPos :
097000100811                      ds_Pos_1.LenDta :
097100100811                      ds_List :
097200100811                      Qusec );
097300100811
097400100811           // -?Impostazione campi di output?
097500100811           WHfldn  += 1;
097600100811           WHfobo   = OutBuf;
097700100811           H1PosFin = OutBuf + FldLen - 1;
097800100811           H1fiob   = Usage;
097900100811           WHfldp   = DecPos;
098000100811           clear  apKEYN;
098100100811
098200100811           select;
098300100811             when  H1EdtCde > *blank;
098400100811               V2EdtWrdD = 'EdtCde';
098500100811               V2EdtWrd  = H1EdtCde;
098600100811             when  EdtLen  > *zero   and
098700100811                   EdtLen <= 30;
098800100811               V2EdtWrdD = 'EdtWrd';
098900100811               V2EdtWrd  = '''' + %subst( EdtWrd : 1 : EdtLen ) + '''';
099000100811             other;
099100100811               clear  V2EdtWrdD;
099200100811               clear  V2EdtWrd;
099300100811           endsl;
099400100811
099500100811           // -?Find key field number. Do this scaning key field name array?
099600101029           Hind43 = *off;
099700100811           if  apNKYF <> *zero;
099800100811             xx = %lookup ( V1FldNam : sch_KFNA );
099900100811             if  xx > *zero;
100000100811               apKEYN = sch_KFNU(xx);
100100101029               Hind43 = (sch_KFSQ(xx) = 'D');
100200100811             endif;
100300100811           endif;
100400100811
100500100811           // -?Edit decimal positions?
100600100811           if  Digits = *zero;
100700100811             WHfldb = FldLen;
100800100811             Hind41 = *on;
100900100811           else;
101000100811             WHfldb = Digits;
101100100811             Hind41 = *off;
101200100811           endif;
101300100811
101400100811           // -?Check S1nrr for the first record of each page?
101500100811           S1nrr += 1;
101600100811           if  %rem ( S1nrr : 16 ) = 1;
101700100811             H1RcdFms = ds_Info_1.RcdFMI;
101800100811           else;
101900100811             clear  H1rcdfms;
102000100811           endif;
102100100811
102200100811           // -?Write subfile?
102300101029           FieldAlfa  = (Hind41 = *on);
102400101029           OrdDescend = (Hind43 = *on);
102500100811           write  DDS_S1;
102600100811           write  DDS_S2;
102700100811
102800100811           $FirstFmt = *off;
102900100811
103000100811           // -?Set next entry start position?
103100100811           ds_Pos_1.StrPos += ds_RcvVar_FFD.LstSiz;
103200100811           w_MaxSflRrn += 1;
103300100811
103400100811         ENDDO;
103500100811
103600100811       ENDSR;
103700100811
103800100811      /TITLE SR_RESETSFL - Reset subfile fields
103900100811
104000100811       //--------------------------------------------------------------
104100100811       //?Reset subfile fields                                         ?
104200100811       //--------------------------------------------------------------
104300100811       BEGSR  sr_ResetSfl;
104400100811
104500100811         // -?Salvataggio posizione del cursore?
104600100811         SaveSflRrn = S1nrr;
104700100811
104800100811         // -?Ciclo di lettura subfile (per eliminare evidenziazioni)?
104900100811         S1nrr = 1;
105000100811         chain  S1nrr  DDS_S1;
105100100811
105200100811         DoW  %found(UBDDSD);
105300100811
105400100811           // -?Reimpostazione attributi di visualizzazione?
105500100811           FieldFound = *off;
105600100811           FileIntest = (H1rcdFms = *all'-');
105700101029           FieldAlfa  = (Hind41 = *on);
105800101029           OrdDescend = (Hind43 = *on);
105900100811
106000100811           update  DDS_S1;
106100100811
106200100811           chain  S1nrr  DDS_S2;
106300100811           FieldFound = *off;
106400100811           FileIntest = (H1rcdFms = *all'-');
106500101029           FieldAlfa  = (Hind41 = *on);
106600101029           OrdDescend = (Hind43 = *on);
106700100811           update  DDS_S2;
106800100811
106900100811           S1nrr += 1;
107000100811           chain  S1nrr  DDS_S1;
107100100811
107200100811         EndDo;
107300100811
107400100811         // -?Reimpostazione SflRrn?
107500100811         S1nrr = SaveSflRrn;
107600100811
107700100811       ENDSR;
107800100811
107900100811      /TITLE SR_CTRLT2 - Verifica dati della videata principale
108000100811
108100100811       //--------------------------------------------------------------
108200100811       //?Check if the file name is changed                            ?
108300100811       //--------------------------------------------------------------
108400100811       BEGSR  sr_CtrlT2;
108500100811
108600100811         // -?Spegnimento degli indicatori di errore?
108700100811         %subst(IndDspF : 50) = *off;
108800100811
108900100811         // -?Reset last file name if blank?
109000100811         if  WHfile = *blank;
109100100811           WHfile = ds_Info_1.FilName;
109200100811         endif;
109300100811
109400100811         // -?Reset last library name if blank?
109500100811         if  WHlib  = *blank;
109600100811           WHlib  = ds_Info_1.LibName;
109700100811         endif;
109800100811
109900100811         // -?Check for file existence if changed?
110000100811         IF  ds_FileLib <> ds_Info_1.FilLib;
110100100811
110200100811           Qcmd = c_CmdCHKOBJ_a
110300100811                + %trimr(WHlib) + '/' + %trimr(WHfile)
110400100811                + c_CmdCHKOBJ_b;
110500100811           exsr  sr_ExecCmd;
110600100811
110700100811           // -?File or library not found?
110800100811           if  Qusei <> *blank;
110900100811             H1riga = c_PosCurFile_row;
111000100811             H1colo = c_PosCurFile_col_File;
111100100811             Atr1        = c_X23;
111200100811             ErrFile     = *on;
111300100811             ErrMessage  = *on;
111400100811             ErrGenerico = *on;
111500100811             V1msg = $Msg(01);
111600100811           else;
111700100811             pi_FilLib = ds_FileLib;
111800100811             pi_FldNap = WHflde;
111900100811             clear  SaveFLDE;
112000100811             $FirstFmt = *on;
112100100811             $InzS1    = *on;
112200100811           endif;
112300100811
112400100811         ENDIF;
112500100811
112600100811       ENDSR;
112700100811
112800100811      /TITLE SR_GEST2 - Elaborazione dati della videata principale
112900100811
113000100811       //--------------------------------------------------------------
113100100811       //?Work with subfile                                            ?
113200100811       //--------------------------------------------------------------
113300100811       BEGSR  sr_GesT2;
113400100811
113500100811         select;
113600100811
113700101026           // -?NON richiesto nulla di nuovo?
113800101026           when  WHflde = *blank   and   SaveFLDE = WHflde;
113900101026             //clear  SaveFLDE;
114000100811             H1riga = c_H1riga_dft;
114100100811             if  Not  GestioneF22;
114200100811               H1colo = c_H1colo_dft_x_Fld;
114300100811             else;
114400100811               H1colo = c_H1colo_dft_x_Str;
114500100811             endif;
114600100811
114700100811           // -?Richiesto posizionamento in ultima pagina?
114800100811           when  WHflde = 'B'   or   WHflde = 'BOT';
114900100811             C1RcdNbr = w_MaxSflRrn;
115000101026             SaveSflRrn = C1RcdNbr;
115100100811
115200100811           // -?Richiesto posizionamento in prima pagina?
115300100811           when  WHflde = 'T'   or   WHflde = 'TOP';
115400100811             C1RcdNbr = 1;
115500101026             SaveSflRrn = C1RcdNbr;
115600100811
115700100811           // -?Richiesto posizionamento per nome campo?
115800100811           //  ?(Scan field name if changed)?
115900101026           //  ?o  tolto campo da ricercare?
116000101026           //when  WHflde <> *blank   and
116100101026           //      WHflde <> SaveFLDE;
116200101026           when  WHflde <> SaveFLDE;
116300100811             exsr  sr_ScanField;
116400101112
116500101112           // -?Lasciato campo da ricercare?
116600101112           when  WHflde = SaveFLDE;
116700101112             // -?Visualizzazione pagina con il record (se trovato)?
116800101112             if  $FldFound;
116900101112               H1riga = RowInt;
117000101112               H1colo = ColInt;
117100101112               C1RcdNbr = SaveSflRrn;
117200101112             endif;
117300100811
117400100811         endsl;
117500100811
117600100811       ENDSR;
117700100811
117800100811      /TITLE SR_SCANFIELD - Scan field name
117900100811
118000100811       //--------------------------------------------------------------
118100100811       //?Scan field name                                              ?
118200100811       //--------------------------------------------------------------
118300100811       BEGSR  sr_ScanField;
118400100811
118500100811         if  NOT $StrFound;
118600100811           ds_FoundString.fs_row = c_H1riga_dft;
118700100811           ds_FoundString.fs_col = c_H1colo_dft_x_Str;
118800100811           InfDspF.min_nrr = 1;
118900100811         endif;
119000100811
119100100811         // -?Salvataggio posizione del cursore?
119200100811         SaveSflRrn = S1nrr;
119300100811
119400100811         // -?Ciclo di lettura subfile (per ricerca campo)?
119500100811         S1nrr      = 1;
119600100811         $FldFound  = *off;
119700100811
119800100811         SaveFLDE   = WHflde;
119900100811         chain  S1nrr  DDS_S1;
120000100811
120100100811         DoW  %found(UBDDSD);
120200100811
120300100811           FieldFound = (WHflde = V1FldNam);
120400100811
120500101026           If  FieldFound   and   NOT  $FldFound;
120600101026             $FldFound  = *on;
120700101026             ErrMessage  = *on;
120800101026             ErrGenerico = *on;
120900101026             V1msg = $Msg(03);
121000101026             // -?Memorizzazione SflRrn del 1° campo corrispondete?
121100101026             SaveSflRrn = S1nrr;
121200100811           EndIf;
121300100811
121400100811           // -?Reimpostazione attributi di visualizzazione?
121500100811           FileIntest = (H1rcdFms = *all'-');
121600101029           FieldAlfa  = (Hind41 = *on);
121700101029           OrdDescend = (Hind43 = *on);
121800100811
121900100811           update  DDS_S1;
122000100811
122100100811           chain  S1nrr  DDS_S2;
122200100811           FieldFound = (WHflde = V1FldNam);
122300100811           FileIntest = (H1rcdFms = *all'-');
122400101029           FieldAlfa  = (Hind41 = *on);
122500101029           OrdDescend = (Hind43 = *on);
122600100811           update  DDS_S2;
122700100811
122800100811           S1nrr += 1;
122900100811           chain  S1nrr  DDS_S1;
123000100811
123100100811         EndDo;
123200100811
123300100811         If  NOT $FldFound;
123400100811           H1riga = c_H1riga_dft;
123500100811           H1colo = c_H1colo_dft_x_Fld;
123600101026           if  WHflde <> *blank;
123700101026             // -?Emissione messaggio di campo NON trovato?
123800101026             Atr2        = c_X23;
123900101026             ErrField    = *on;
124000101026             ErrMessage  = *on;
124100101026             ErrGenerico = *on;
124200101026             V1msg = $Msg(04);
124300101026           endif;
124400100811         Else;
124500100811           // -?Visualizzazione pagina con il record (se trovato)?
124600100811           H1riga = RowInt;
124700100811           H1colo = ColInt;
124800100811           C1RcdNbr = SaveSflRrn;
124900100811         EndIf;
125000100811
125100100811         // -?Reimpostazione SflRrn?
125200100811         S1nrr = SaveSflRrn;
125300100811
125400100811       ENDSR;
125500100811
125600100811      /TITLE SR_SCANSTRING - Scan string
125700100811
125800100811       //--------------------------------------------------------------
125900100811       //?Scan string                                                  ?
126000100811       //--------------------------------------------------------------
126100100811       BEGSR  sr_ScanString;
126200100811
126300100811         if  NOT $StrFound;
126400100811           ds_FoundString.fs_row = c_H1riga_dft;
126500100811           ds_FoundString.fs_col = c_H1colo_dft_x_Str;
126600100811           InfDspF.min_nrr = 1;
126700100811         endif;
126800100811
126900100811         // -?Salvataggio posizione del cursore?
127000100811         SaveSflRrn = S1nrr;
127100100811
127200100811         // -?Controllo se immessa stringa da ricercare?
127300100811         if  V1string = *blank;
127400100811           ErrMessage  = *on;
127500100811           ErrGenerico = *on;
127600100811           V1msg = $Msg(05);
127700100811           leavesr;
127800100811         endif;
127900100811
128000100811         // -?Impostazioni iniziali?
128100100811         H1riga = InfDspF.Cursor_Row;
128200100811         H1colo = InfDspF.Cursor_Col;
128300100811         clear  $StrFound;
128400100811
128500100811         // -?Calcolo Nrr record del subfile da leggere?
128600100811         select;
128700100811           // -?Normale proseguimento?
128800100811           when  H1riga > 6;
128900100811             select;
129000100811               when  InfDspf.dsp_aid = c_F16;
129100100811                 S1nrr += 1;
129200100811               when  InfDspf.dsp_aid = c_F17;
129300100811                 S1nrr -= 1;
129400100811             endsl;
129500100811           // -?F16 = Ricerca avanti (riparte dall'inizio)?
129600100811           when  InfDspf.dsp_aid = c_F16;
129700100811             S1nrr = 1;
129800100811           // -?F17 = Ricerca indietro (riparte dalla fine)
129900100811           when  InfDspf.dsp_aid = c_F17;
130000100811             if  InfDspF.min_nrr = 1;
130100100811               S1nrr = w_LastSflRrn;
130200100811             else;
130300100811               S1nrr = InfDspF.min_nrr + c_SflPag;
130400100811             endif;
130500100811         endsl;
130600100811
130700100811         // -?Ciclo di lettura record del subfile?
130800100811         chain  S1nrr  DDS_S1;
130900100811
131000100811         DOW  %found(UBDDSD)   and
131100100811              NOT $StrFound;
131200100811
131300100811           QDCXLATE ( %len(H1text) :
131400100811                      H1text :
131500100811                      'QSYSTRNTBL' :
131600100811                      'QSYS' );
131700100811
131800100811           w_StrPos = %scan( %trimr( V1string ) : H1text );
131900100811           $StrFound = (w_StrPos > *zero);
132000100811
132100100811           // -?Stringa trovata:?
132200100811           //  ?Visualizzazione pagina con il record (se trovata)?
132300100811           If  $StrFound;
132400100811             H1riga = %rem(S1nrr : c_SflPag);
132500100811             if  H1riga = *zero;
132600100811               H1riga = c_SflPag;
132700100811             endif;
132800100811             H1riga += 6;
132900100811             H1colo = w_StrPos + 33;
133000100811             C1RcdNbr = S1nrr;
133100100811             SaveSflRrn = S1nrr;
133200100811             ds_FoundString.fs_row = H1riga;
133300100811             ds_FoundString.fs_col = H1colo;
133400100811             ErrMessage  = *on;
133500100811             ErrGenerico = *on;
133600100811             V1msg = $Msg(06);
133700100811             leavesr;
133800100811           EndIf;
133900100811
134000100811           // -?Lettura record successivo/precedente del subfile?
134100100811           select;
134200100811             when  InfDspf.dsp_aid = c_F16;
134300100811               S1nrr += 1;
134400100811             when  InfDspf.dsp_aid = c_F17;
134500100811               S1nrr -= 1;
134600100811           endsl;
134700100811
134800100811           chain  S1nrr  DDS_S1;
134900100811
135000100811         ENDDO;
135100100811
135200100811         // -?Stringa NON trovata!?
135300100811         If  S1nrr > w_LastSflRrn   or
135400100811             S1nrr = *zero;
135500100811
135600100811           ErrMessage  = *on;
135700100811           ErrGenerico = *on;
135800100811           if  S1nrr > w_LastSflRrn;
135900100811             V1msg = $Msg(07);
136000100811           else;
136100100811             V1msg = $Msg(08);
136200100811           endif;
136300100811
136400100811           // -?Ripristino posizione del cursore?
136500100811           S1nrr  = SaveSflRrn;
136600100811           H1riga = c_H1riga_dft;
136700100811           H1colo = c_H1colo_dft_x_Str;
136800100811
136900100811           leavesr;
137000100811
137100100811         EndIf;
137200100811
137300100811       ENDSR;
137400100811
137500100811      /TITLE SR_PRINTD - Print definition
137600100811
137700100811       //--------------------------------------------------------------
137800100811       //?Print definition                                             ?
137900100811       //--------------------------------------------------------------
138000100811       BEGSR  sr_PrintD;
138100100811
138200100811         // -?Override printer file?
138300100811         Qcmd = c_CmdOVRPRTF
138400100811              + %trimr(WHfile)
138500100811              + ')';
138600100811         exsr  sr_ExecCmd;
138700100811
138800100811         // -?Apertura printer file?
138900100811         open  PrintDDS;
139000100811
139100100811         // -?Impostazioni iniziali?
139200100811         SaveSflRrn = S1nrr;
139300100811         InfPrtF.OvrFlw = 66;
139400100811         clear  Page;
139500100811         clear  w_SaveRcdFms;
139600100811
139700100811         // -?Reperimento orario iniziale?
139800100811         s_Time = %dec( %time() );
139900100811
140000100811         // -?Ciclo di lettura del subfile?
140100100811         S1nrr = 1;
140200100811         chain  S1nrr  DDS_S1;
140300100811
140400100811         DOW  %found(UBDDSD);
140500100811
140600100811           if  H1RcdFms <> *blank   and
140700100811               H1RcdFms <> w_SaveRcdFms;
140800100811             w_SaveRcdFms =  H1RcdFms;
140900100811           endif;
141000100811
141100100811           // -?Test overflow?
141200100811           if  InfPrtF.OvrFlw > 60;
141300100811             except  PrtTxt;
141400100811             if  H1RcdFms <> *all'-';
141500100811               except  PrtRcdFmt;
141600100811             endif;
141700100811           endif;
141800100811
141900100811           // -?Stampa dettaglio?
142000100811           if  H1RcdFms = *all'-';
142100100811             except  PrtRcdFmP;
142200100811           else;
142300101029             *in41 = (Hind41 = *on);
142400101029             *in43 = (Hind43 = *on);
142500100811             except  PrtFld;
142600100811           endif;
142700100811
142800100811           S1nrr += 1;
142900100811           chain  S1nrr  DDS_S1;
143000100811
143100100811         ENDDO;
143200100811
143300100811         // -?Stampa "fine stampa"?
143400100811         except  PrtEnd;
143500100811
143600100811         // -?Chiusura printer file?
143700100811         close  PrintDDS;
143800100811
143900100811         // -?Ripristino posizione del cursore?
144000100811         S1nrr = SaveSflRrn;
144100100811
144200100811         // -?Emissione messaggio di avvenuta stampa?
144300100811         ErrMessage = *on;
144400100811         V1msg = $Msg(09);
144500100811
144600100811       ENDSR;
144700100811
144800100811      /TITLE SR_OMIT - Visualizza, con DSPFD, le Select e le Omit
144900100811
145000100811       //--------------------------------------------------------------
145100100811       //?Select/Omit del file                                         ?
145200100811       //--------------------------------------------------------------
145300100811       BEGSR  sr_Omit;
145400100811
145500100811         Qcmd = c_CmdDSPFD_a
145600100811              + %trimr(WHlib) + '/' + %trimr(WHfile)
145700100811              + c_CmdDSPFD_c;
145800100811
145900100811         exsr  sr_ExecCmd;
146000100811
146100100811       ENDSR;
146200100811
146300100811      /TITLE SR_ROUTEND - Operazioni finali
146400100811
146500100811       //--------------------------------------------------------------
146600100811       //?Operazioni finali.                                           ?
146700100811       //--------------------------------------------------------------
146800100811       BEGSR  sr_RoutEnd;
146900100811
147000100811         return;
147100100811
147200100811       ENDSR;
147300100811
147400100811       //--------------------------------------------------------------
147500100811       //?Esecuzione del comando (già impostato).                      ?
147600100811       //--------------------------------------------------------------
147700100811       BEGSR  sr_ExecCmd;
147800100811
147900100811         clear Qcap0100;
148000100811         Qcabcsdh = *off;
148100100811         Qcapa    = *off;
148200100811         Qcacmdss = *off;
148300100811         Qcaerved = *allX'00';
148400100811
148500100811         clear Qusec;
148600100811         Qusbprv  = %size(Qusec);
148700100811
148800100811         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
148900100811                           %size(Qcap0100) : 'CPOP0100' : *omit :
149000100811                           0 : 0 : Qusec);
149100100811
149200100811         //if  Qusei <> *blank;
149300100811         //  ...;
149400100811         //endif;
149500100811
149600100811       ENDSR;
149700100811
149800100811      /end-free
149900100811
150000100811      /TITLE File di stampa
150100100811
150200100811       //--------------------------------------------------------------
150300100811       //?File di stampa.                                              ?
150400100811       //--------------------------------------------------------------
150500100811
150600100811     oPrintDDS  e            PrtTxt            2
150700100811     o                                              'Tracciato record file'
150800100811     o                       WHfile           +   1
150900100811     o                       *Date            +  10 '  /  /    '
151000100811     o                       s_Time           +  10 '  :  :  '
151100100811     o                                        +  10 '('
151200100811     o                       SDSpgm           +   0
151300100811     o                                        +   1 '© by +
151400100811     o                                              Gruppo PRO & +
151500100811     o                                              SETRAS Srl'
151600100811     o                                        +   0 ')'
151700100811     o                                          128 'Pag.'
151800100811     o                       Page             +   0
151900100811      *
152000100811     o          e            PrtTxt      2
152100100811     o                                              'File ....:'
152200100811     o                       WHfile           +   1
152300100811     o                                        +   7 'Testo: '''
152400100811     o                       s_ObjTxt         +   0
152500100811     o                                        +   0 ''''
152600100811     o                                        +   5 'Tipo file:'
152700100811     o                       T1ftyp           +   1
152800100811      *
152900100811     o          e            PrtTxt      1
153000100811     o                                              'Libreria :'
153100100811     o                       WHlib            +   3
153200100811     o                                        +  10 'Lunghezza record:'
153300100811     o                       WHrlen        j  +   1
153400100811     o                                        +  10 'Numero totale campi:'
153500100811     o                       WHfldn        j  +   1
153600110126     o              n39                       +  10 'Tipo/Ordinam. Key:'
153700110126     o              n39      T1keyo           +   1
153800100811      *
153900100811     o          e            PrtTxt      1
154000100811     o                                              'Formato/i:'
154100100811     o                       s_WHname         +   1
154200100811      *
154300100811     o          e            PrtTxt      2
154400100811     o                                           48 '--Posizione--'
154500100811     o                                           57 'Lung.'
154600100811     o                                           61 'T'
154700100811     o                                           68 'Pos.'
154800100811      *
154900100811     o          e            PrtTxt      1
155000100811     o                                            4 'Key'
155100100811     o                                           15 'Formato'
155200100811     o                                           26 'Campo'
155300100811     o                                           40 'Da'
155400100811     o                                           48 'A'
155500100811     o                                           57 'campo'
155600100811     o                                           61 'c'
155700100811     o                                           68 'dec.'
155800100811     o                                           82 'Testo campo'
155900100811     o                                          125 'Uso'
156000100811     o                                          132 'EdtCde'
156100100811      *
156200100811     o          e            PrtTxt      1
156300100811     o                                           22 '----------------------'
156400100811     o                       *place              44
156500100811     o                       *place              66
156600100811     o                       *place              88
156700100811     o                       *place             110
156800100811     o                       *place             132
156900100811      *
157000100811     o          E            PrtRcdFmt   2  1
157100100811     o                       w_SaveRcdFms        18
157200100811      *
157300100811     o          E            PrtRcdFmP   1
157400100811     o                       V1FldNam            18
157500100811     o                       H1RcdFms            31
157600100811     o                       H1text             121
157700100811      *
157800100811     o          E            PrtFld      1
157900100811     o                       apKEYN        z      4
158000101029     o               43                        +  0 '-Desc'
158100100811     o                       V1FldNam            31
158200100811     o                       WHfobo        1     40
158300100811     o                       H1PosFin      1     48
158400100811     o                       WHfldb        1     57
158500100811     o                       V1DtaTyp            61
158600100811     o              N41      WHfldp        1     67
158700100811     o                       H1text             121
158800100811     o                       H1fiob             124
158900100811     o                       H1EdtCde           130
159000100811      *
159100100811     o          E            PrtEnd           64
159200100811     o                                           75 '***  Fine Lista  ***'
159300100811
159400100811       //--------------------------------------------------------------
159500100811       //?Schiere a tempo di compilazione.                             ?
159600100811       //--------------------------------------------------------------
159700100811
159800100811** - $Msg -------------------------------------------------------------------*
159900100811Libreria / File inesistente.                                                   1
160000100811Elaborazione in corso. Attendere, prego...                                     2
160100100811Trovato campo.                                                                 3
160200100811Non trovato campo.                                                             4
160300100811Immettere la stringa da ricercare.                                             5
160400100811Trovata stringa.                                                               6
160500100811Non trovata stringa prima della fine del file.                                 7
160600100811Non trovata stringa prima dell'inizio del file.                                8
160700100811La stampa è stata eseguita con successo.                                       9
