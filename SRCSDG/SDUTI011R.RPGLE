000100151002     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO)
000200151002     H*PARMS BNDDIR(PJXBND PJCBND)
000300151002     H*PARMS ACTGRP(QILE)
000400151006     H*****************************************************************
000500151002     H DECEDIT('0,') DATEDIT(*DMY.)
000600151006     H*****************************************************************
000700000000     H*
000800000000     H*               -------------------------------
000900151002     H* Dalla lettura di Anso100f:
001000151002     H*
001100151006     H* Stampa elenco soggetti esclusi dalla black list solamente per alcune attività.
001200151006     H* Flag di Esclusione parziale nella G12 e soggetto presente in anso100f
001300151002     H*
001400151002     H* Scrittura del file SDGSO300F (nuovi dati soggetto)
001500151002     H* Preimposto i campi dai dati soggetto e poi dalle variazioni fiscali ed infine
001600151002     H* da Anso100f.
001700151002     H* Se nel file anso100f sono presenti i dati del soggetto rappresentato,
001800151002     H* vanno scritti quelli.
001900151002     H* Se Invece è presente solo il nome e il cognome del soggetto devo reperire
002000151002     H* gli altri dati utili da ansog e anind
002100000000     H*
002200000000     F*----------------------------------------------------*
002300151002     F* anagrafica soggetti standard
002400151002     FANSO101L  IF   E           K DISK
002500151002     FANSOG01L  IF   E           K DISK
002600151002     FANIND01L  IF   E           K DISK
002700151006     FANRCO03l  IF   E           K DISK
002800151006     FANFRN01L  IF   E           K DISK
002900151002     F*------------
003000151002     FSDGSO301L UF A E           K DISK
003100151002     F*------------
003200151006     F* Printer file lista soggetti da escludere
003300151006     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003400000000     D*-------------
003500000000     D* Reperimento nome PGM
003600000000     D STATUS         SDS           333
003700000000     D  DSPGM            *PROC
003800000000     D*------------------
003900000000     D* DS Esterna gestione messaggi
004000000000     D SDGMSGDS      E DS
004100000000     D  STS          E                     EXTFLD(MSGSTS)
004200000000     D                                     DIM(333)
004300000000     D  JBA          E                     EXTFLD(MSGJBA)
004400000000     D                                     DIM(502)
004500151002     D*-------------
004600151002     D* Passaggio Parametri
004700151002     D KPJBA         E DS
004800000000     D*------------------
004900151001     D* Dati di ambiente ottenuti da SDGSOCDR
005000151001     D SOC001        E DS                  EXTNAME(SDGSOCDS )
005100151001     D*-------------
005200151001     D* DS Interna per dati di output di SDGSOCDR
005300151001     D XSOCDS          DS          1000
005400151006     D*-------------
005500151006     D* Richiamo a XATB
005600151006     D XATBDS        E DS                  Inz
005700151006     D*-------------
005800151006     D* tabella G12 - Stati
005900151006     D AngG12DS      E DS                  Inz
006000151001     D*-------------
006100151002     D* Variazioni estremi fiscali
006200151002     D ANCPI00F      E DS
006300151002     D*-------------
006400151002     D* Driver estremi fiscali (ANCPI00F)
006500151002     D DVPRic          S              1
006600151002     D DVPSocieta      S              3
006700151002     D DVPUnita        S              8
006800151002     D DVPStrutt       S             10
006900151002     D DVPDtRif        S               D
007000151002     D DVPLenOut       S              9B 0
007100151002     D DVPSogg         S              8
007200151002     D DVPErrore       S              1
007300151002     D DVPOutput       S           4000
007400151002     D*---------
007500000000     D*------------------
007600151001     D UDateISO        S               D
007700151001     D DateIso         S               D   inz
007800151001     D Sys             S              3  0 inz
007900151001     D NrasReg         S              9  0 inz
008000151006     D $Stato          S              4
008100151006     D $Soggetto       S              8
008200160303     D $dtIdbl         S              6  0
008300160303     D $dtFdbl         S              6  0
008400160303     D $Descrizione    S             25
008500151006     D $DesFor         S             19
008600151006     D $Piva           S             20
008700151006     D $CdFisc         S             20
008800151006     D $Uic            S              3
008900151006     D $Kcc            S              6
009000151006     D $Ksc            S              8
009100000000     C*----------------------------------------------------*
009200000000     C*                MAIN LINE PROGRAM
009300000000     C*----------------------------------------------------*
009400150930     C*
009500151002     C                   EXSR      ELABORA
009600000000     C*
009700000000     C                   EXSR      ENDPGM
009800151002     C/EJECT
009900151002     C************************************************************
010000151002     C* Elabora
010100151002     C************************************************************
010200151002     C     Elabora       BEGSR
010300151002     C*
010400151002     C     *loval        setll     AnSO101l
010500151002     C*
010600151002     C                   Do        *hival
010700151002     C*
010800151002     C                   Read      AnSO101l
010900151002     C                   If        %eof(Anso101l)
011000151002     C                   Leave
011100151002     C                   Endif
011200151002     C*
011300151002     C* Verifico che non esista già
011400151002     C*
011500151002     C                   Clear                   SO3DTFIVL
011600151002     C                   Clear                   SO3SYS
011700151002     C                   Clear                   SO3NRASINT
011800151002     C                   Eval      So3Sogg = So1Sogg
011900151002     C*
012000151002     C     K04SO301      CHAIN     SDGSO301L
012100151002     C                   If        Not %Found(SDGSO301L)
012200151002     C                   Exsr      Aggiorna
012300151002     C                   Exsr      Esclude
012400151002     C                   Endif
012500151002     C*
012600151002     C                   Enddo
012700151002     C
012800151002     C                   ENDSR
012900000000     C/EJECT
013000000000     C************************************************************
013100000000     C* Esegui aggiornamenti
013200000000     C************************************************************
013300151002     C     Aggiorna      BEGSR
013400151002     C*
013500151002     C* da ANAGRAFICA SOGGETTI STANDARD
013600151002     C     So1Sogg       CHAIN     ANSOG01L                           21
013700151002     C                   if        *In21       = *Off
013800151002     C*
013900151002     C     So1Sogg       CHAIN     ANIND01L                           21
014000151002     C                   if        *In21       = *Off
014100151002     C*
014200151002     C                   CLEAR                   SDGSO3000
014300151002     C* imposto la data e l'utente di immissione
014400151002     C                   Eval      SO3DtImm    = UDateIso
014500151002     C                   Eval      SO3UteImm   = XScCUt
014600151002     C                   Eval      SO3DtUlMo   = UDateIso
014700151002     C                   Eval      SO3UteUlMo  = XScCUt
014800151002     C*
014900151002     C                   Eval      SO3Sogg     = SO1Sogg
015000151002     C* Preimposto dai dati anagrafici
015100151002     C                   Eval      SO3PartIVA  = SOGPartIVA
015200151002     C                   Eval      SO3CdFisc   = SOGCdFisc
015300151002     C                   Eval      SO3CogRag   = SOGDes
015400151002     C                   Eval      SO3Nome     = *Blank
015500151002     C                   Eval      SO3Sex      = SOGSex
015600151002     C                   Eval      SO3DtNasc   = SOGDtNasc
015700151002     C                   Eval      SO3LocNasc  = *Blank
015800151002     C                   Eval      SO3PrvNasc  = *Blank
015900151002     C                   Eval      SO3Stato    = INDStato
016000151002     C                   Eval      SO3StaFed   = *Blank
016100151002     C                   Eval      SO3Localit  = INDLocalit
016200151002     C                   Eval      SO3IndirE   = INDIndriz
016300151002     C* Reperisco i dati delle variazioni fiscali alla data odierna
016400151002     C                   Exsr      RepAncpi
016500151002     C*
016600151002     C                   If        SO1Cognome <> *blanks
016700151002     C                   Eval      SO3CogRag   = SO1Cognome
016800151002     C                   Eval      SO3Nome     = SO1Nome
016900151002     C                   Eval      SO3Stato    = SO1StaNa
017000151002     C                   Eval      SO3PrvNasc  = SO1PrvNa
017100151002     C                   Eval      SO3LocNasc  = SO1ComNa
017200151002     C                   Endif
017300151002     C* Dati Soggetto rappresentato
017400151002     C                   If        SO1RpRagSo <> *blanks or
017500151002     C                             SO1RpCogn  <> *blanks
017600151002     C                   Eval      SO3Stato    = So1RpStato
017700151002     C                   Eval      SO3Localit  = So1RpLocal
017800151002     C                   Eval      SO3Indire   = So1RpInd
017900151002     C                   Eval      SO3Partiva  = So1RpCIva
018000151002     C                   Eval      SO3CdFisc   = So1RpCFis
018100151002     C                   If        SO1RpRagSo <> *blanks
018200151002     C                   Eval      SO3CogRag   = So1RpRagSo
018300151002     C                   else
018400151002     C                   Eval      SO3CogRag   = So1RpCogn
018500151002     C                   Eval      SO3Nome     = So1RpNome
018600151002     C                   Eval      SO3DtNasc   = So1RpDtNa
018700151002     C                   Eval      SO3LocNasc  = So1RpComNa
018800151002     C                   Eval      SO3PrvNasc  = So1RpPrNa
018900151002     C                   Eval      SO3Sex      = So1RpSesso
019000151002     C                   Endif
019100151002     C                   Endif
019200151006     D* se non valorizzate nè Partita Iva nè Codice fiscale, devo
019300151006     D* usare - come codice fiscale - l'identificativo estero
019400151006     C* memorizzato su ANSO1
019500151006     C                   If        SO3PartIva = *Blanks And
019600151006     C                             SO3CdFisc  = *Blanks
019700151006     C                   Eval      SO3CdFisc  = So1Flg4
019800151006     C                   EndIf
019900151002     C*
020000151006     C                   EndIf
020100160303     C                   EndIf
020200151006     C* Scrivo solo se ho dei dati, il reperimento mi serve ugualmente
020300160303     C                   If        SO1COGNOME <> *blanks  or
020400160303     C                             SO1NOME    <> *blanks  or
020500160303     C                             SO1COMNA   <> *blanks  or
020600160303     C                             SO1PRVNA   <> *blanks  or
020700160303     C                             SO1STANA   <> *blanks  or
020800160303     C                             SO1CAPNA   <> *blanks  or
020900160303     C                             SO1RPRAGSO <> *blanks  or
021000160303     C                             SO1RPCOGN  <> *blanks  or
021100160303     C                             SO1RPNOME  <> *blanks  or
021200160303     C                             SO1RPDTNA  <> *loval   or
021300160303     C                             SO1RPCOMNA <> *blanks  or
021400160303     C                             SO1RPPRNA  <> *blanks  or
021500160303     C                             SO1RPSTNA  <> *blanks  or
021600160303     C                             SO1RPCAPNA <> *blanks  or
021700160303     C                             SO1RPSESSO <> *blanks  or
021800160303     C                             SO1RPSTATO <> *blanks  or
021900160303     C                             SO1RPPROV  <> *blanks  or
022000160303     C                             SO1RPLOCAL <> *blanks  or
022100160303     C                             SO1RPIND   <> *blanks  or
022200160303     C                             SO1RPCIVA  <> *blanks  or
022300160303     C                             SO1RPCFIS  <> *blanks
022400151002     C*
022500151002     C                   WRITE     SDGSO3000                            22
022600151002     C*
022700151006     C                   Endif
022800151002     C* se rcd non scrivibile attivo errore
022900151002     C     *IN22         IFEQ      *ON
023000000000     C                   EXSR      MSGPGM
023100000000     C                   ENDIF
023200000000     C*
023300000000     C                   ENDSR
023400151002     C/EJECT
023500151002     C************************************************************
023600160303     C* Stampa i soggetti esclusi o inclusi con i relativi Kcc/Ksc
023700151002     C************************************************************
023800151002     C     Esclude       BEGSR
023900151006     C* Verifico dallo stato reperito se nella G12 è presente il flag
024000151006     C* di esclusione parziale.
024100151006     C*
024200151006     C                   Movel     SO3Stato      $stato
024300151006     C                   Exsr      RepTabG12
024400151006     C*
024500160303     C                   If        BlackLG12 ='2'  and
024600160303     C                             SO1Blackl <> *blanks
024700151006     C* Reperisco i conti del soggetto
024800151006     C                   Eval      RCOSocieta = Xscsoc
024900151006     C                   Eval      RCOSogg    = SO3Sogg
025000151006     C                   Eval      RCOSnatura = 'F'
025100151006     C*
025200151006     C     K03RCO03      Setll     AnRCO03l
025300151006     C*
025400151006     C                   Do        *Hival
025500151006     C     K03RCO03      Reade     AnRCO03l
025600151006     C*
025700151006     C                   If        %eof(AnRCO03l)
025800151006     C                   Leave
025900151006     C                   Endif
026000151006     C*
026100151006     C                   Exsr      RiePrt
026200151006     C*
026300151006     C                   If        *inof = *on
026400151006     C                   Except    Testa
026500151006     C                   setoff                                       OF
026600151006     C                   Endif
026700151006     C*
026800151006     C                   Except    Riga
026900160303     C*
027000160303     C                   If        *inof = *on
027100160303     C                   Except    Testa
027200160303     C                   setoff                                       OF
027300160303     C                   Endif
027400160303     C*
027500160303     C                   Select
027600160303     C                   When      So1Blackl = '1'
027700160303     C                   move      010101        $dtidbl
027800160303     C                   move      311299        $dtfdbl
027900160303     C                   If        So1DtIDbl <> *loval
028000160303     C     *jobrun       Move      So1DtIDbl     $dtidbl
028100160303     C                   Endif
028200160303     C                   If        So1DtFDbl <> *hival and
028300160303     C                             So1DtFDbl <> *loval
028400160303     C     *jobrun       Move      So1DtFDbl     $dtFdbl
028500160303     C                   Endif
028600160303     C                   Except    Rigescl
028700160303     C
028800160303     C                   When      So1Blackl = '0'
028900160303     C                   Except    Rigincl
029000160303     C*
029100160303     C                   EndSl
029200151006     C*
029300151006     C                   Enddo
029400151006     C*
029500151006     C                   EndIf
029600151006     C*
029700151002     C                   ENDSR
029800151002     C/EJECT
029900151006     C************************************************************
030000151006     C* Reperisco i dati di tabella G12
030100151006     C************************************************************
030200151006     C     RepTabG12     BEGSR
030300151006     C*
030400151006     C                   Clear                   ANGG12DS
030500151006     C*
030600151006     C                   Clear                   XATBDS
030700151006     C                   Move      '1'           XTBRIC
030800151006     C                   Move      Xscsoc        XTBAZI
030900151006     C                   Move      *ZERO         XTBKEY
031000151006     C                   Move      'G12'         XTBCOD
031100151006     C                   Move      $Stato        XTBKEY
031200151006     C                   Callb     'XATB'
031300151006     C                   Parm                    XATBDS
031400151006     C*
031500151006     C                   If        XTbErr = '0'
031600151006     C                   Movel     XTBUNI        AngG12DS
031700151006     C                   Else
031800151006     C                   Clear                   ANGG12DS
031900151006     C                   EndIf
032000151006     C*
032100151006     C                   ENDSR
032200151006     C/EJECT
032300151006     C************************************************************
032400151006     C* Riempio i campi di stampa
032500151006     C************************************************************
032600151006     C     Rieprt        BEGSR
032700151006     C*
032800151006     C                   eval      FRNSocieta   =  Xscsoc
032900151006     C                   eval      FRNKsc       =  RcoKsc
033000151006     C*
033100151006     C     K02FRN01      Chain     AnFRN01l
033200151006     C*
033300151006     C                   If        %found(anfrn01l)
033400151006     C                   Eval      $DesFor      =  FRNDesBrev
033500151006     C                   Else
033600151006     C                   Clear                   $DesFor
033700151006     C                   Endif
033800151006     C*
033900151006     C                   Eval      $Soggetto    =  SO3Sogg
034000151006     C                   Eval      $Descrizione =  SogDes
034100151006     C                   Eval      $Piva        =  SO3Partiva
034200151006     C                   Eval      $CdFisc      =  SO3CdFisc
034300151006     C                   Eval      $Uic         =  SO3CodUic
034400151006     C                   Eval      $Kcc         =  RcoKcc
034500151006     C                   Eval      $Ksc         =  RcoKsc
034600151006     C*
034700151006     C                   ENDSR
034800151006     C/EJECT
034900151002     C************************************************************
035000151002     C* Reperimento dati anagrafici da dati storicizzati
035100151002     C************************************************************
035200151002     C     RepANCPI      BEGSR
035300151002     C*
035400151002     C*
035500151002     C                   Eval      DVPLenOut = %size(ANCPI00F)
035600151002     C*
035700151002     C                   CALLB     'NDDVACPI'
035800151002     C                   PARM      '2'           DVPRic
035900151002     C                   PARM      XscSoc        DVPSocieta
036000151002     C                   PARM      *Blank        DVPUnita
036100151002     C                   PARM      'ANCPI     '  DVPStrutt
036200151002     C                   PARM      UDATEISO      DVPDtRif
036300151002     C                   PARM                    DVPLenOut
036400151002     C                   PARM      SO1Sogg       DVPSogg
036500151002     C                   PARM      *Off          DVPErrore
036600151002     C                   PARM                    DVPOutPut
036700151002     C*
036800151002     C                   if        DVPErrore   = *Off
036900151002     C                   Eval      %subst(ANCPI00F:1:DVPLenout)
037000151002     C                              = %subst(DVPOutput:1:DVPLenout)
037100151002     C*
037200151002     C* trovati dati storicizzati alla data di oggi, sovrascrivo
037300151002     C* quelli validi attualmente per il soggetto
037400151002     C                   MoveL(P)  CPICdFisc     SO3CdFisc
037500151002     C                   MoveL(P)  CPIPartIva    SO3PartIva
037600151002     C                   MoveL(P)  CPIDesSog1    SO3CogRag
037700151002     C*
037800151002     C                   MoveL     CPIStato      SO3Stato
037900151002     C                   MoveL(P)  CPILocalit    SO3Localit
038000151002     C                   MoveL(P)  CPIIndriz     SO3Indire
038100151002     C*
038200151002     C                   endif
038300151002     C*
038400151002     C                   ENDSR
038500000000     C/EJECT
038600000000     C************************************************************
038700000000     C* Messaggio errore
038800000000     C************************************************************
038900000000     C     MsgPgm        BEGSR
039000000000     C*
039100000000     C                   Reset                   SDGMSGDS
039200000000     C*
039300000000     C                   Movel     'N'           MsgRsp
039400000000     C                   Movel     *Blanks       MsgTpR
039500000000     C                   Z-Add     0             MsgMsg
039600000000     C                   Movel     'S'           MsgEmV
039700000000     C                   Z-Add     10            MsgRgp
039800000000     C                   Movel     'N'           MsgLck
039900000000     C                   Movea     Status        Sts
040000000000     C                   Movel     'N'           MsgCnl
040100000000     C                   Movel     'N'           MsgVid
040200000000     C                   Movel     'N'           MsgOpe
040300000000     C                   Movel     'N'           MsgStp
040400000000     C*
040500000000     C                   Call      'SDGMSGR'
040600000000     C                   Parm                    SDGMSGDS
040700000000     C                   Parm                    XMSGTxt         135
040800000000     C                   Parm                    XMSGInz           1
040900000000     c* codici dei 3 messaggi possibili da emettere a video
041000000000     C* "Si sono verificati errori che hanno impedito l'esecuzione"
041100000000     C                   PARM      'COS0377'     MSGCd1            7
041200000000     C                   PARM                    MSGCd2            7
041300000000     C                   PARM                    MSGCd3            7
041400000000     c* codice del messaggio da mandare alla coda dell'operatore
041500000000     C                   PARM                    MSGSn1            7
041600000000     c* codici(lunghi 7  ciascuno) dei 9 bottoni
041700000000     C                   PARM                    MSGCdTxt         63
041800000000     c* codici(lunghi 7  ciascuno) dei 2 titoli (titolo e sottotitolo)
041900000000     c* per stampa e video
042000000000     C                   PARM                    MSGCdTit         14
042100000000     c* flag per chiusura prtf
042200000000     C                   PARM                    MSGClPrtf         1
042300000000     C* variabili per messaggi
042400000000     C                   PARM                    VarMsg1         100
042500000000     C                   PARM                    VarMsg2         100
042600000000     C                   PARM                    VarMsg3         100
042700000000     C*
042800000000     C                   ENDSR
042900000000     C/EJECT
043000000000     C************************************************************
043100000000     C* OPERAZIONI INIZIALI
043200000000     C************************************************************
043300000000     C     *INZSR        BEGSR
043400000000     C*
043500151001     C     *DMY          Move      Udate         UDateISO
043600151001     C*
043700151006     C* Reperimento dati società
043800151006    >C                   MOVEL     'SOC001'      TIPXSC
043900151006     C                   MOVEL     *BLANK        SOCXSC
044000151001     C                   Exsr      RepSoc
044100151006     C     RTNXSC        IFNE      '1'
044200151006     C                   MOVEL     XSOCDS        SOC001
044300151006     C                   ELSE
044400151006     C                   RETURN
044500151006     C                   ENDIF
044600151006     C*
044700151006     C                   Seton                                        OF
044800151002     C*
044900151002     C                   ENDSR
045000151002     C/EJECT
045100151002     C************************************************************
045200151002     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
045300151002     C************************************************************
045400151002     C     DEFCAM        BEGSR
045500151002     C*
045600151002     C* klist
045700151002     C*
045800151002     C* SDGSO301L
045900151002     C     K04SO301      KLIST
046000151002     C                   KFLD                    SO3Sogg
046100151002     C                   KFLD                    SO3DtFiVL
046200151002     C                   KFLD                    SO3Sys
046300151002     C                   KFLD                    SO3NrAsint
046400151006     C*
046500151006     C* ANRCO03L
046600151006     C     K03RCO03      KLIST
046700151006     C                   KFLD                    RCOSogg
046800151006     C                   KFLD                    RCOSocieta
046900151006     C                   KFLD                    RCOSnatura
047000151006     C*
047100151006     C* ANFRN01L
047200151006     C     K02FRN01      KLIST
047300151006     C                   KFLD                    FRNSocieta
047400151006     C                   KFLD                    FRNKSC
047500151002     C                   ENDSR
047600151002     C/EJECT
047700151001     C************************************************************
047800151001     C* REPERIMENTO DATI SOCIETA'
047900151001     C************************************************************
048000151001     C     REPSOC        BEGSR
048100151001     C*
048200151001     C                   CALL      'SDGSOCDR'
048300151001     C                   PARM                    TIPXSC            6
048400151001     C                   PARM                    SOCXSC            3
048500151001     C                   PARM                    CDSXSC            9 0
048600151001     C                   PARM                    MODXSC            3
048700151001     C                   PARM      *BLANKS       RTNXSC            1
048800151001     C                   PARM                    XSOCDS
048900151001     C                   PARM                    KPJBA
049000151001     C*
049100151001     C                   ENDSR
049200151001     C/EJECT
049300000000     C************************************************************
049400000000     C* Fine programma
049500000000     C************************************************************
049600000000     C     ENDPGM        BEGSR
049700000000     C*
049800000000     C                   SETON                                        LR
049900000000     C                   Return
050000000000     C*
050100000000     C                   ENDSR
050200000000     C/EJECT
050300151006     C************************************************************
050400151006     OQSYSPRT   E            TESTA            02
050500151006     O                                           35 '***---'
050600151006     O                                         +  0 '------------------------'
050700151006     O                                         +  0 '------------------------'
050800151006     O                                         +  0 '--***'
050900151006     O          E            TESTA            03
051000151006     O                                           35 '***---'
051100160303     O                                         +  0 ' Black list:soggetti da '
051200160303     O                                         +  0 'escludere o includere  -'
051300151006     O                                         +  0 '--***'
051400151006     O          E            TESTA            04
051500151006     O                                           35 '***---'
051600151006     O                                         +  0 '------------------------'
051700151006     O                                         +  0 '------------------------'
051800151006     O                                         +  0 '--***'
051900151006     O*
052000151006     O          E            TESTA            06
052100160303     O                                            3 'Soc'
052200160303     O                                           13 'Soggetto'
052300160303     O                                           26 'Descrizione'
052400160303     O                                           51 'P. Iva'
052500160303     O                                           78 'Cod. Fiscale'
052600160303     O                                           92 'Stato'
052700160303     O                                           96 'UIC'
052800160303     O                                          106 'Fornitore'
052900160303     O                                          124 'Descrizione'
053000160303     O*
053100160303     O          E            Riga        1
053200160303     O                       Xscsoc               3
053300160303     O                       $Soggetto           13
053400160303     O                       $Descrizione        40
053500160303     O                       $Piva               65
053600160303     O                       $CdFisc             86
053700160303     O                       $Stato              91
053800160303     O                       $Uic                06
053900160303     O                       $Kcc               103
054000160303     O                       $Ksc               112
054100160303     O                       $DesFor            132
054200160303     O*
054300160303     O          E            Rigincl     1
054400160303     O                                           13 'Escluso'
054500160303     O          E            Rigescl     1
054600160303     O                                           15 'Incluso da'
054700160303     O                       $DtIdbl             26
054800160303     O                                           28 'a'
054900160303     O                       $DtFdbl             39
055000160303     O*
