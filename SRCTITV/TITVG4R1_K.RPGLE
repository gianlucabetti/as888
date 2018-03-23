000100060225      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR/FIVAXWWR.
000200990908     H dftactgrp(*yes)
000300060228     H DECEDIT('0,') DATEDIT(*DMY.)
000400990908
000500000724     Fazorg01l  if   e           k disk
000600041210     Ftabel00f  if   e           k disk
000700990910     Ftivin00r  uF   E             DISK    usropn
000800021113     FFIVABwwr  O    E             DISK    usropn
000900021113     FFIVATwwr  O    E             DISK    usropn
001000060227     FFIVAXwwr  O    E             DISK    usropn
001100041206     Ftitvg4p   O    f  132        PRINTER usropn
001200000621     F                                     oflind(*inoa)
001300041206     Ftitvg4ps  O    f  198        PRINTER usropn
001400000621     F                                     oflind(*inob)
001500990908
001600000512     D*------------
001700000512     D* COMANDI
001800000512     D*------------
001900011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002000041210     D*------------
002100041210     D* SCHIERE
002200041210     D*------------
002300041210     D skNAZISO        S              3    DIM(1000)
002400041210     D skNAZBAR        S              3    DIM(1000)
002500060227     D skNAZBARDES     S             25    DIM(1000)
002600000801     D*----------------------------------------------------
002700000801     D* DICHIARAZIOINE VARIABILI DI WRK
002800000801     D*----------------------------------------------------
002900990920     D dscmz         e ds                  inz
003000990910     D psds           sds
003100990910     D  procname         *PROC
003200990920     D tivlrds       e ds                  extname(tivlr00f)
003300041210     D ds15          e ds
003400990910     D esito           s              1
003500000724     D prmlit          s             10
003600000710     D prmfir          s             10
003700990921     D wrkesito        s                   like(esito)
003800990915     D wrkdata         s               d
003900990915     D wrkora          s               t
004000000613     D rrnum           s              6  0 INZ(*zeros)
004100000621     D recko           s            150    INZ(*blanks)
004200011113     D depcmd          s            150    INZ(*blanks)
004300041210     D parccm          s              8    INZ(*blanks)
004400010202     D parmbr          s             10    INZ(*blanks)
004500010202     D paropz          s              1    INZ(*blanks)
004600010202     D chkcall         s              1    INZ(*blanks)
004700041210     D FlgNewBol       s              1    INZ(*blanks)
004800041210     D jNAZ            s              5  0 INZ(*zeros)
004900000830
005000000830     D*------------------
005100000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
005200000830     D*------------------
005300000830     D WLBDA8          DS                  INZ
005400000830     D  G08DAT                 1      8  0
005500000830     D  G08INV                 9     16  0
005600000830     D  G08ERR                17     17
005700000830     D  G08TGI                18     22  0
005800041025     D*------------------
005900041025     D* DS REPERIMENTO NUMERATORE
006000041025     D*------------------
006100041025     D trul33ds      e ds                  inz
006200041025     D*------------------
006300041025     D* DS ARCHITETTURA
006400041025     D*------------------
006500041025     D kpjba         e ds                  inz
006600041025     D*------------------
006700060225
006800060225     D*------------------
006900060225     D* DEFINIZIONE DS D WRK X CONFORMAZIONE LAYOUT PACKING LIST
007000060225     D*------------------
007100060227     D DS__T_1         DS                  INZ
007200060225     D  sT1_LOGO                     30
007300060225     D  sT1_MITT                     25
007400060225     D  sT1_BLK1                      5
007500060225     D  sT1_DEST                     55
007600060225     D*
007700060227     D DS__T_2         DS                  INZ
007800060227     D  sT2_TES1_1                   20
007900060227     D  sT2_TES1_2                   35
008000060225     D  sT2_BLK1                      5
008100060227     D  sT2_TES2_1                   20
008200060227     D  sT2_TES2_2                   35
008300060227     D*
008400060227     D DS__R_1         DS                  INZ
008500060227     D  sR1_SEP                     116    INZ(*all'_')
008600060227     D*
008700060227     D DS__R_2         DS                  INZ
008800060227     D  sR2_SEP                     116    INZ(*all'¯')
008900060227     D*
009000060227     D DS__R_3         DS                  INZ
009100060227     D  sR3_SEP                     116    INZ(*all'=')
009200060227     D*
009300060227     D DS__D_1         DS                  INZ
009400060227     D  sD1_CONST1                   16    INZ('Posizione')
009500060227     D  sD1_CONST2                   40    INZ('Codice materiale')
009600060228     D  sD1_CONST3                   10    INZ('Qtà')
009700060228     D  sD1_CONST4                   24    INZ('Ubicazione magazzino')
009800060228     D  sD1_CONST5                   12    INZ('Qtà ordinata')
009900060228     D  sD1_CONST6                    2    INZ
010000060228     D  sD1_CONST7                   12    INZ('Qtà residua')
010100060227     D*
010200060227     D DS__D_2         DS                  INZ
010300060227     D  sD2_BLK1                     16
010400060227     D  sD2_CONST1                   40    INZ('Descrizione')
010500060228     D  sD2_BLK2                     10
010600060228     D  sD2_BLK3                     24
010700060228     D  sD2_BLK4                     12
010800060228     D  sD2_BLK5                      2
010900060228     D  sD2_BLK6                     12
011000060227     D*
011100060227     D DS__D_3         DS                  INZ
011200060227     D  sD3_BLK1                     16
011300060227     D  sD3_CONST1                   40    INZ('Spedizione effettuata per conto-
011400060228     D                                      di:')
011500060228     D  sD3_BLK2                     10
011600060228     D  sD3_CONST2                   24    INZ('Da consegnare a:')
011700060228     D  sD3_BLK3                     12
011800060228     D  sD3_BLK4                      2
011900060228     D  sD3_BLK5                     12
012000060227     D*
012100060227     D DS__D_4         DS                  INZ
012200060228     D  sD4_BLK1                     20
012300060227     D  sD4_MITT                     40
012400060228     D  sD4_BLK2                     10
012500060227     D  sD4_VETT                     40
012600060227     D*
012700060227     D DS__D_5         DS                  INZ
012800060228     D  sD5_DESCR                   116
012900060227     D*
013000060227     D DS__D_6         DS                  INZ
013100060227     D  sD6_POSNUM                   16
013200060227     D  sD6_MATNUM                   40
013300060228     D  sD6_QTA                      10
013400060228     D  sD6_BLK1                     24
013500060228     D  sD6_QTAORD                   12
013600060228     D  sD6_BLK2                      2
013700060228     D  sD6_QTARES                   12
013800060228     D*
013900060228     D DS__D_7         DS                  INZ
014000060228     D  sD7_BLK1                     16
014100060228     D  sD7_DETMATDES                40
014200060228     D*
014300060228     D DS__D_8         DS                  INZ
014400060228     D  sD8_BLK1                     16
014500060228     D  sD8_CONST1                   20    INZ('Nr. ordine cliente:')
014600060228     D  sD8_CLIORDNUM                35
014700060228     D  sD8_BLK2                      5
014800060228     D  sD8_CONST2                    5    INZ('del:')
014900060228     D  sD8_CLIORDDAT                10
015000060228     D*
015100060228     D DS__D_9         DS                  INZ
015200060228     D  sD9_BLK1                     16
015300060228     D  sD9_CONST1                   10    INZ('N. part:')
015400060228     D  sD9_DETBARCOD                10
015500060228     D  sD9_BLK2                      5
015600060228     D  sD9_CONST2                   12    INZ('Data scad:')
015700060228     D  sD9_DATSCAD                  30
015800060227     D*
015900060227     D DS__E_1         DS                  INZ
016000060227     D  sE1_CONST1                  100    INZ('INFORMAZIONI SU RESIDUO DA EVAD-
016100060228     D                                     ERE')
016200060227     D*
016300060227     D DS__E_2         DS                  INZ
016400060227     D  sE2_CONST1                   12    INZ('Nr.Doc. ')
016500060227     D  sE2_CONST2                    7    INZ('Pos.')
016600060227     D  sE2_CONST3                   20    INZ('Materiale')
016700060227     D  sE2_CONST4                   42    INZ('Descrizione')
016800060227     D  sE2_CONST5                   17    INZ('Qtà Ordinata')
016900060227     D  sE2_CONST6                   17    INZ('Qtà Residua')
017000060227     D*
017100060227     D DS__E_3         DS                  INZ
017200060227     D  sE3_NUMORD                   12
017300060227     D  sE3_POS                       7
017400060227     D  sE3_MATNUM                   20
017500060227     D  sE3_MATDES                   42
017600060227     D  sE3_QTAORD                   17
017700060227     D  sE3_QTADIF                   17
017800060228     D*
017900060228     D DS__P_1         DS                  INZ
018000060228     D  sP1_DICITUR1                 15
018100060228     D  sP1_VALORE1                  25
018200060228     D  sP1_BLK1                      5
018300060228     D  sP1_DICITUR2                 40
018400060228     D  sP1_VALORE2                  12
018500060227     D*
018600060225
018700060225     D*------------------
018800060225     D* DEFINIZIONE DS D WRK X RIDEFINIZIONE TIPI RECORD FILE CLIENTE
018900060225     D*------------------
019000060225     D DS_A1           DS                  INZ
019100060225     D  dA1_NUMORD                   10
019200060225     D  dA1_POS                       6
019300060225     D  dA1_MATNUM                   18
019400060225     D  dA1_MATDES                   40
019500060225     D  dA1_QTAORD                   15
019600060225     D  dA1_QTADIF                   15
019700060225     D  dA1_UNIT                      3
019800060228     D*
019900060228     D wDS_A1          DS                  INZ
020000060228     D  wdA1_NUMORD                  10
020100060228     D  wdA1_POS                      6
020200060228     D  wdA1_MATNUM                  18
020300060228     D  wdA1_MATDES                  40
020400060228     D  wdA1_QTAORD                  15
020500060228     D  wdA1_QTADIF                  15
020600060228     D  wdA1_UNIT                     3
020700060225     D*
020800060225     D DS_A2           DS                  INZ
020900060225     D  dA2_CONTAT                   40
021000060225     D  dA2_TEL                      30
021100060225     D  dA2_FAX                      30
021200060225     D*
021300060225     D DS_A3           DS                  INZ
021400060225     D  dA3_MITTID                   17
021500060225     D  dA3_MITTRSD                  40
021600060225     D  dA3_MITTIND                  40
021700060225     D  dA3_MITTCAP                  10
021800060225     D  dA3_MITTLOC                  40
021900060225     D  dA3_MITTNAZ                   3
022000060225     D*
022100060225     D DS_A4           DS                  INZ
022200060227     D  dA4_DESTID                   17
022300060225     D  dA4_DESTRSD                  40
022400060225     D  dA4_DESTRD2                  40
022500060225     D  dA4_DESTIND                  40
022600060225     D  dA4_DESTCAP                  10
022700060225     D  dA4_DESTLOC                  40
022800060225     D  dA4_DESTNAZ                   3
022900060225     D  dA4_DESTPRV                   3
023000060225     D*
023100060225     D DS_A5           DS                  INZ
023200060225     D  dA5_ORIGRSD                  40
023300060225     D  dA5_ORIGIND                  40
023400060225     D  dA5_ORIGCIV                   4
023500060225     D  dA5_ORIGCAP                  10
023600060225     D  dA5_ORIGLOC                  40
023700060227     D  dA5_ORIGNAZ                   3
023800060228     D  dA5_ORIGNAZD                 25
023900060225     D*
024000060225     D DS_A6           DS                  INZ
024100060228     D  dA6_DATSPE                   10
024200060225     D*
024300060225     D DS_A7           DS                  INZ
024400060227     D  dA7_VETTRSD                  40
024500060227     D  dA7_VETTIND                  40
024600060227     D  dA7_VETTCAP                  10
024700060227     D  dA7_VETTLOC                  40
024800060227     D  dA7_VETTNAZ                   3
024900060225     D*
025000060225     D DS_A8           DS                  INZ
025100060225     D  dA8_NUMDOC                   10
025200060225     D  dA8_PORTO1                    3
025300060225     D  dA8_PORTO2                   28
025400060225     D  dA8_ROUTE                     6
025500060225     D  dA8_PESOTOT                  17
025600060225     D  dA8_PESONET                  15
025700060225     D  dA8_PESOUNIT                  3
025800060225     D  dA8_VOLTOT                   15
025900060225     D  dA8_VOLUNIT                   3
026000060225     D  dA8_COLLI                     5
026100060225     D*
026200060225     D DS_B1           DS                  INZ
026300060225     D  dB1_QTADIF                   15
026400060225     D  dB1_UNIT                      3
026500060225     D  dB1_QTAORD                   15
026600060225     D*
026700060225     D DS_B2           DS                  INZ
026800060225     D  dB2_CLIORDNUM                35
026900060228     D  dB2_CLIORDDAT                10
027000060225     D*
027100060225     D DS_B3           DS                  INZ
027200060225     D  dB3_CLIORDNUM                35
027300060228     D  dB3_CLIORDDAT                10
027400060225     D*
027500060225     D DS_B4           DS                  INZ
027600060225     D  dB4_DATSCAD                  30
027700060225     D*
027800060225     D DS_B4_1         DS                  INZ
027900060225     D  dB4_1_DATSCAD                30
028000060228     D*
028100060228     D wDS_B4          DS                  INZ
028200060228     D  wdB4_DATSCAD                 30
028300060225     D*
028400060225     D DS_B5           DS                  INZ
028500060228     D***  dB5_IDOCNUM                  35
028600060228     D  dB5_IDOCNUM                  10
028700060225     D*
028800060225     D DS_B6           DS                  INZ
028900060225     D  dB6_DETPOSNUM                 6
029000060225     D  dB6_DETMATNUM                18
029100060225     D  dB6_DETMATDES                40
029200060225     D  dB6_DETBARCOD                10
029300060225     D  dB6_DETQTASPE                15
029400060225     D  dB6_DETUNIT                   3
029500060228     D*
029600060228     D wDS_B6          DS                  INZ
029700060228     D wdB6_DETPOSNUM                 6
029800060228     D wdB6_DETMATNUM                18
029900060228     D wdB6_DETMATDES                40
030000060228     D wdB6_DETBARCOD                10
030100060228     D wdB6_DETQTASPE                15
030200060228     D wdB6_DETUNIT                   3
030300060225     D*
030400060225
030500060225     D*------------------
030600060225     D* DEFINIZIONE SCHIERE X CONFORMAZIONE LAYOUT PACKING LIST
030700060225     D*------------------
030800060228     D skVAR_PKB6      S                   DIM(100) LIKE(vaxDTT) INZ(*blanks)   * schiera Packings
030900060228     D skVAR_PKB4      S                   DIM(100) LIKE(vaxDTT) INZ(*blanks)   * schiera Packings
031000060228     D skVAR_PKB4_1    S                   DIM(100) LIKE(vaxDTT) INZ(*blanks)   * schiera Packings
031100060228     D jVAR_PK         S              3  0 INZ(*zeros)                          * suo indice
031200060228     D skVAR_RS        S                   DIM(100) LIKE(vaxDTT) INZ(*blanks)   * schiera Residui
031300060227     D jVAR_RS         S              3  0 INZ(*zeros)                          * suo indice
031400990908
031500010201
031600010201
031700990915     C                   time                    wrkdata
031800990915     C                   time                    wrkora
031900000913     C                   reset                   rrnum
032000990921     C                   reset                   esito
032100990921     C                   reset                   wrkesito
032200000724     C*
032300000724     C* SE OCCORRE SPEDIRE IN FILIALE
032400000724     C                   if        vlrpoi <> *zeros
032500000724     C*
032600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
032700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
032800000724     C     vlrpoi        chain     azorg01l
032900041210     C                   if        %found(azorg01l)
033000000616     C                   movel(p)  CMD(1)        depcmd
033100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
033200000616     C*
033300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
033400011113     C                   Z-ADD     150           LENGH            15 5
033500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
033600000616     C                   PARM                    depcmd
033700000616     C                   PARM                    LENGH
033800000724     C*
033900000724     C                   endif
034000000724     C                   endif
034100000616     C*
034200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
034300000616     C                   movel(p)  CMD(2)        depcmd
034400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
034500000616     C*
034600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
034700011113     C                   Z-ADD     150           LENGH            15 5
034800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
034900000616     C                   PARM                    depcmd
035000000616     C                   PARM                    LENGH
035100000616     C*
035200041206     C                   if        not %open(titvg4ps)
035300041206     C                   open      titvg4ps
035400000616     C                   except    testdett
035500000613     C                   endif
035600000613     C*
035700041210     C                   EXSR      CARTAB                                       CARICA TABELLE
035800040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
035900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
036000000613     C*
036100010202     C* Effettuo la chiamata al CLLE preposto
036200060225     C                   call(e)   'TITVVTXC'
036300010202     C                   parm                    parccm
036400010202     C                   parm                    parmbr
036500010202     C                   parm      '2'           paropz
036600010202     C*
036700041206     C                   if        %open(titvg4ps)
036800000616     C                   except    findett
036900041206     C                   close     titvg4ps
037000000613     C                   endif
037100000616     C*
037200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
037300011113     C                   Z-ADD     150           LENGH            15 5
037400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
037500000616     C                   PARM                    CMD(3)
037600000616     C                   PARM                    LENGH
037700000616     C*
037800010201     C                   seton                                        LR
037900000613
038000000613
038100000613     C*--------------------------------------------------------
038200000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
038300000613     C*--------------------------------------------------------
038400000621     C     STPR          BEGSR
038500000613     C*
038600041206     C                   if        not %open(titvg4p)
038700041206     C                   open      titvg4p
038800990915     C                   endif
038900990915     C*
039000990915     C                   except    riepilogo
039100990915     C*
039200041206     C                   if        %open(titvg4p)
039300041206     C                   close     titvg4p
039400990914     C                   endif
039500990910     C*
039600000613     C                   ENDSR
039700000613     C***
039800041210
039900041210     C*--------------------------------------------------------
040000041210     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
040100041210     C*--------------------------------------------------------
040200041210     C     CARTAB        BEGSR
040300041210     C*
040400041210     C* TABELLA '15' - NAZIONI
040500041210     C                   clear                   skNAZISO
040600041210     C                   clear                   skNAZBAR
040700060227     C                   clear                   skNAZBARDES
040800041210     C                   eval      tblKUT = 1
040900041210     C                   eval      tblCOD = '15'
041000041210     C     KEYtabP       setll     tabel00f
041100041210     C     KEYtabP       reade     tabel00f
041200041210     C                   dow       not %eof(tabel00f)
041300041210     C                   if        tblFLG = *blanks
041400041210     C                   movel(p)  tblUNI        ds15
041500041210     C                   add       1             jNAZ
041600041210     C                   eval      skNAZBAR(jNAZ) = tblKEY
041700060227     C                   eval      skNAZBARDES(jNAZ) = §15DES
041800041210     C                   eval      skNAZISO(jNAZ) = §15COD
041900041210     C                   endif
042000041210     C     KEYtabP       reade     tabel00f
042100041210     C                   enddo
042200041210     C*
042300041210     C                   ENDSR
042400041210     C***
042500041210
042600041210
042700041210
042800910830     C*--------------------------------------------------------
042900060225     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR e FIVAXWWR  *
043000910830     C*--------------------------------------------------------
043100040526     C     RWFILE        BEGSR
043200990910     C*
043300990914     C                   if        not %open(tivin00r)
043400990908     C                   open      tivin00r
043500990914     C                   endif
043600021113     C                   if        not %open(fivabwwr)
043700021113     C                   open      fivabwwr
043800990914     C                   endif
043900021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
044000060225     C                   exsr      prevatvax
044100010201     C*
044200010202     C                   if        chkcall = '0'
044300010202     C*
044400021113     C                   if        not %open(fivatwwr)
044500021113     C                   open      fivatwwr
044600010201     C                   endif
044700060225     C*
044800060225     C                   if        not %open(fivaxwwr)
044900060225     C                   open      fivaxwwr
045000060225     C                   endif
045100990910     C*
045200010201     C                   clear                   §CTROKVB          5 0
045300020305     C                   clear                   §CTROKVT          5 0
045400060225     C                   clear                   §CTROKVX          5 0
045500000801     C                   clear                   §CTRMO            5 0
045600000801     C                   clear                   §CTRNO            5 0
045700040910     C*
045800921023     C                   DO        *HIVAL
045900990913     C*
046000990915     C                   READ      tivin00r                               70
046100040910     C                   if        vindta > *blanks
046200000613     C                   add       1             rrnum
046300000801     C*
046400000801     C                   if        *in70 = *off
046500000801     C                             and
046600000801     C                             (vinflg = *blanks
046700000801     C                              or vinflg = '0'
046800000801     C                              or vinflg = '2')
046900000801     C*
047000000801     C                   clear                   vinmsg
047100000801     C                   eval      vinflg = '1'
047200040910     C*
047300040910     C* Eseguo routine d traduzione
047400060225     C                   exsr      impvbvtvx
047500040802     C*
047600010305     C                   endif
047700000905     C*
047800000905     C                   else
047900000905     C                   eval      vinflg = '1'
048000000905     C                   endif
048100000905     C*
048200000905     C  N70              update    tivin000
048300000905     C*
048400991022     C  N70              ENDdo
048500041210     C*
048600060227     C* Scarico i buffer eventualmente rimasti in sospeso
048700041210     C                   IF        FlgNewBol = '1'
048800041210     C                   WRITE     FIVAB000
048900041210     C                   IF        VATNOT <> *blanks
049000041210     C                   WRITE     FIVAT000
049100041210     C                   ENDIF
049200060227     C                   EXSR      WRIVAX
049300041210     C                   ENDIF
049400010202     C*
049500010202     C                   endif
049600990910
049700990910     C* Se non ci sono record con errori ...
049800000710     C                   if        §ctrno = 0
049900990910     C* ... restituisco esito OK.
050000990921     C                   eval      wrkesito = '0'
050100990910     C                   else
050200010201     C                   if        §ctrokvb > 0
050300990921     C                   eval      wrkesito = '1'
050400000710     C                   else
050500000710     C                   eval      wrkesito = '2'
050600990910     C                   endif
050700000710     C                   endif
050800990910     C*
050900990914     C                   if        %open(tivin00r)
051000990908     C                   close     tivin00r
051100990914     C                   endif
051200021113     C                   if        %open(fivabwwr)
051300021113     C                   close     fivabwwr
051400990914     C                   endif
051500021113     C                   if        %open(fivatwwr)
051600021113     C                   close     fivatwwr
051700010201     C                   endif
051800060225     C                   if        %open(fivaxwwr)
051900060225     C                   close     fivaxwwr
052000060225     C                   endif
052100990910     C*
052200010201     C                   if        §ctrokvb > 0
052300000724     C                             and vlrpoi <> *zeros
052400010202     C                   exsr      invio
052500990920     C                   endif
052600990920     C*
052700910830     C                   ENDSR
052800000613     C***
052900990920
053000000801     C*----------------------------------------------------*
053100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053200000801     C*----------------------------------------------------*
053300010201     C     INZVAR        BEGSR
053400000801     C*
053500040802     C                   Z-ADD     *zeros        Num5_0            5 0
053600040802     C                   MOVEL     '0'           FlgCAS            1
053700060227     C*
053800060227     C* Inizializzo tutte le schiere e le ds d wrk
053900060227     C                   CLEAR                   DS_A1
054000060227     C                   CLEAR                   DS_A2
054100060227     C                   CLEAR                   DS_A3
054200060227     C                   CLEAR                   DS_A4
054300060227     C                   CLEAR                   DS_A5
054400060227     C                   CLEAR                   DS_A6
054500060228     C***                CLEAR                   DS_A7
054600060227     C                   CLEAR                   DS_A8
054700060227     C                   CLEAR                   DS_B1
054800060227     C                   CLEAR                   DS_B2
054900060227     C                   CLEAR                   DS_B3
055000060227     C                   CLEAR                   DS_B4
055100060227     C                   CLEAR                   DS_B4_1
055200060227     C                   CLEAR                   DS_B5
055300060227     C                   CLEAR                   DS_B6
055400060227     C*
055500060228     C                   CLEAR                   skVAR_PKB4
055600060228     C                   CLEAR                   skVAR_PKB4_1
055700060228     C                   CLEAR                   skVAR_PKB6
055800060227     C                   Z-ADD     *zeros        jVAR_PK
055900060228     C                   CLEAR                   skVAR_RS
056000060227     C                   Z-ADD     *zeros        jVAR_RS
056100000801     C*
056200000801     C                   ENDSR
056300000801     C*----------------------------------------------------*
056400040910     C*  IMPOSTAZIONE CAMPI COSTANTI
056500000801     C*----------------------------------------------------*
056600000801     C     DEFCAM        BEGSR
056700000801     C*
056800021113     C                   CLEAR                   FIVAB000
056900040802     C                   CLEAR                   FIVAT000
057000060225     C                   CLEAR                   FIVAX000
057100020619     C* Imposto i valori di default...
057200041210     C                   Z-ADD     0431893       VABCCM
057300041210     C                   Z-ADD     0431893       VATCCM
057400060225     C                   Z-ADD     0431893       VAXCCM
057500041210     C                   Z-ADD     043           VABLNP
057600041210     C                   Z-ADD     043           VATLNP
057700060225     C                   Z-ADD     043           VAXLNP
057800041210     C                   Z-ADD     000           VABCTR
057900060228     C                   MOVEL     '7A'          VABCTM
058000040823     C                   MOVEL     '1'           VABCBO
058100020619     C* ... e poi verifico se sono stati passati come parametri
058200020619     C                   IF        vlrppt > *blanks
058300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
058400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
058500020619     C                   EXSR      CHKNUM
058600020619     C                   IF        PiInt=*on
058700020619     C                   Z-ADD     PiVal         VABCCM
058800020619     C                   Z-ADD     PiVal         VATCCM
058900060225     C                   Z-ADD     PiVal         VAXCCM
059000020619     C                   ENDIF
059100040506     C                   ENDIF
059200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
059300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
059400020619     C                   EXSR      CHKNUM
059500020619     C                   IF        PiInt=*on
059600020619     C                   Z-ADD     PiVal         VABLNP
059700020619     C                   Z-ADD     PiVal         VATLNP
059800060225     C                   Z-ADD     PiVal         VAXLNP
059900040506     C                   ENDIF
060000020619     C                   ENDIF
060100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
060200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
060300020619     C                   EXSR      CHKNUM
060400020619     C                   IF        PiInt=*on
060500020619     C                   Z-ADD     PiVal         VABCTR
060600040506     C                   ENDIF
060700020619     C                   ENDIF
060800041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
060900041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
061000041210     C                   ENDIF
061100020619     C                   ENDIF
061200000801     C*
061300000801     C                   ENDSR
061400000801     C*----------------------------------------------------*
061500060225     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT e FIVAX)
061600000801     C*----------------------------------------------------*
061700060225     C     IMPVBVTVX     BEGSR
061800041210     C*
061900041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
062000041210     C                   Z-ADD     *zeros        errore            1 0
062100060227     C*
062200060227     C********
062300060227     C* Tipo record 'Z1EDL14'
062400060227     C********
062500060227     C                   IF        %trim(%subst(vindta:1:10)) = 'Z1EDL14'
062600060227     C* Valorizzazione => DS_A1
062700060228     C                   EVAL      dA1_NUMORD   = %trim(%subst(vindta:63+1:10))
062800060228     C                   MOVE(P)   dA1_NUMORD    wNum10_0         10 0
062900060228     C                   EVALR     dA1_NUMORD   = %trim(%editc(wNum10_0:'4'))
063000060227     C                   EVAL      dA1_POS      = %trim(%subst(vindta:73+1:6))
063100060228     C                   EVALR     dA1_MATNUM   = %trim(%subst(vindta:79+1:18))
063200060227     C                   MOVE(P)   dA1_MATNUM    wNum18_0         18 0
063300060228     C                   EVALR     dA1_MATNUM   = %trim(%editc(wNum18_0:'4'))
063400060227     C                   EVAL      dA1_MATDES   = %trim(%subst(vindta:97+1:40))
063500060228     C                   EVALR     dA1_QTAORD   = %trim(%subst(vindta:137+1:15))
063600060227     C                   EVAL      PiStr=dA1_QTAORD
063700060227     C                   EXSR      CHKNUM
063800060228     C                   IF        PiNum=*on
063900060228     C                   IF        PiInt=*on
064000060228     C                   Z-ADD     PiVal         wNum30_0         30 0
064100060228     C                   EVALR     dA1_QTAORD   = %trim(%editc(wNum30_0:'2'))
064200060228     C                   ELSE
064300060228     C                   Z-ADD     PiVal         wNum30_3         30 3
064400060228     C                   EVALR     dA1_QTAORD   = %trim(%editc(wNum30_3:'2'))
064500060228     C                   ENDIF
064600060228     C                   ENDIF
064700060228     C                   EVALR     dA1_QTADIF   = %trim(%subst(vindta:152+1:15))
064800060227     C                   EVAL      PiStr=dA1_QTADIF
064900060227     C                   EXSR      CHKNUM
065000060228     C                   IF        PiNum=*on
065100060228     C                   IF        PiInt=*on
065200060228     C                   Z-ADD     PiVal         wNum30_0         30 0
065300060228     C                   EVALR     dA1_QTADIF   = %trim(%editc(wNum30_0:'2'))
065400060228     C                   ELSE
065500060228     C                   Z-ADD     PiVal         wNum30_3         30 3
065600060228     C                   EVALR     dA1_QTADIF   = %trim(%editc(wNum30_3:'2'))
065700060228     C                   ENDIF
065800060228     C                   ENDIF
065900060227     C                   EVAL      dA1_UNIT     = %trim(%subst(vindta:167+1:3))
066000060227     C*
066100060227     C* Trattandosi d record "ripetuto" memorizzo in specifica schiera
066200060228     C                   ADD       1             jVAR_RS
066300060228     C                   EVAL      skVAR_RS(jVAR_RS) = DS_A1
066400060227     C                   ENDIF
066500060227     C*
066600060227     C********
066700060227     C* Tipo record 'Z1EDL20'
066800060227     C********
066900060227     C                   IF        %trim(%subst(vindta:1:10)) = 'Z1EDL20'
067000060227     C* Valorizzazione => DS_A2
067100060227     C                   EVAL      dA2_CONTAT   = %trim(%subst(vindta:130+1:40))
067200060227     C                   EVAL      dA2_TEL      = %trim(%subst(vindta:170+1:30))
067300060227     C                   EVAL      dA2_FAX      = %trim(%subst(vindta:200+1:30))
067400060227     C                   ENDIF
067500060227     C*
067600060227     C********
067700060227     C* Tipo record 'E1ADRM1'
067800060227     C* - soto tipo 'AG' => Mittente
067900060227     C********
068000060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM1' AND
068100060227     C                             %trim(%subst(vindta:64:3)) = 'AG'
068200060227     C* Valorizzazione => DS_A3
068300060228     C                   EVALR     dA3_MITTID   = %trim(%subst(vindta:67+1:17))
068400060227     C                   MOVE(P)   dA3_MITTID    wNum17_0         17 0
068500060228     C                   EVALR     dA3_MITTID   = %trim(%editc(wNum17_0:'4'))
068600060227     C                   EVAL      dA3_MITTRSD  = %trim(%subst(vindta:118+1:40))
068700060227     C                   EVAL      dA3_MITTIND  = %trim(%subst(vindta:438+1:40))
068800060227     C                   EVAL      dA3_MITTCAP  = %trim(%subst(vindta:572+1:10))
068900060227     C                   EVAL      dA3_MITTLOC  = %trim(%subst(vindta:607+1:40))
069000060227     C                   EVAL      dA3_MITTNAZ  = %trim(%subst(vindta:937+1:3))
069100060227     C                   ENDIF
069200060227     C*
069300060227     C********
069400060227     C* Tipo record 'E1ADRM1'
069500060227     C* - soto tipo 'OSO' => Mittente originale
069600060227     C********
069700060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM1' AND
069800060227     C                             %trim(%subst(vindta:64:3)) = 'OSO'
069900060227     C* Valorizzazione => DS_A5
070000060227     C                   EVAL      dA5_ORIGRSD  = %trim(%subst(vindta:118+1:40))
070100060227     C                   EVAL      dA5_ORIGIND  = %trim(%subst(vindta:438+1:40))
070200060227     C                   EVAL      dA5_ORIGCIV  = %trim(%subst(vindta:558+1:4))
070300060227     C                   EVAL      dA5_ORIGCAP  = %trim(%subst(vindta:572+1:10))
070400060227     C                   EVAL      dA5_ORIGLOC  = %trim(%subst(vindta:607+1:40))
070500060227     C                   EVAL      dA5_ORIGNAZ  = %trim(%subst(vindta:937+1:3))
070600060227     C                   Z-ADD     1             jNAZ
070700060227     C     dA5_ORIGNAZ   LOOKUP    skNAZISO(jNAZ)                         55
070800060227     C                   IF        %found
070900060227     C                   EVAL      dA5_ORIGNAZD = skNAZBARDES(jNAZ)
071000060227     C                   ENDIF
071100060227     C                   ENDIF
071200060227     C*
071300060227     C********
071400060227     C* Tipo record 'E1EDT13'
071500060227     C* - soto tipo '007' => IDoc Delivery
071600060227     C********
071700060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDT13' AND
071800060227     C                             %trim(%subst(vindta:64:3)) = '007'
071900060227     C* Valorizzazione => DS_A6
072000060227     C                   EVAL      dA6_DATSPE   = %trim(%subst(vindta:90+1:8))
072100060227     C                   EVAL      dA6_DATSPE   = %subst(dA6_DATSPE:7:2)+ '.'+
072200060227     C                                            %subst(dA6_DATSPE:5:2)+ '.'+
072300060227     C                                            %subst(dA6_DATSPE:1:4)
072400060227     C                   ENDIF
072500060227     C*
072600060227     C********
072700060227     C* Tipo record 'E1ADRM4'
072800060227     C* - soto tipo 'SP' => Vettore
072900060227     C********
073000060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM4' AND
073100060227     C                             %trim(%subst(vindta:64:3)) = 'SP'
073200060227     C* Valorizzazione => DS_A7
073300060227     C                   EVAL      dA7_VETTRSD  = %trim(%subst(vindta:118+1:40))
073400060227     C                   EVAL      dA7_VETTIND  = %trim(%subst(vindta:438+1:40))
073500060227     C                   EVAL      dA7_VETTCAP  = %trim(%subst(vindta:572+1:10))
073600060227     C                   EVAL      dA7_VETTLOC  = %trim(%subst(vindta:607+1:40))
073700060227     C                   EVAL      dA7_VETTNAZ  = %trim(%subst(vindta:937+1:3))
073800060227     C                   ENDIF
073900060227     C*
074000060227     C********
074100060227     C* Tipo record 'Z1EDL24'
074200060227     C********
074300060227     C                   IF        %trim(%subst(vindta:1:10)) = 'Z1EDL24'
074400060227     C* Valorizzazione => DS_B1
074500060228     C                   EVALR     dB1_QTADIF   = %trim(%subst(vindta:164+1:15))
074600060227     C                   EVAL      PiStr=dB1_QTADIF
074700060227     C                   EXSR      CHKNUM
074800060228     C                   IF        PiNum=*on
074900060228     C                   IF        PiInt=*on
075000060228     C                   Z-ADD     PiVal         wNum30_0         30 0
075100060228     C                   EVALR     dB1_QTADIF   = %trim(%editc(wNum30_0:'2'))
075200060228     C                   ELSE
075300060228     C                   Z-ADD     PiVal         wNum30_3         30 3
075400060228     C                   EVALR     dB1_QTADIF   = %trim(%editc(wNum30_3:'2'))
075500060228     C                   ENDIF
075600060228     C                   ENDIF
075700060227     C                   EVAL      dB1_UNIT     = %trim(%subst(vindta:179+1:3))
075800060228     C                   EVALR     dB1_QTAORD   = %trim(%subst(vindta:208+1:15))
075900060227     C                   EVAL      PiStr=dB1_QTAORD
076000060227     C                   EXSR      CHKNUM
076100060228     C                   IF        PiNum=*on
076200060228     C                   IF        PiInt=*on
076300060228     C                   Z-ADD     PiVal         wNum30_0         30 0
076400060228     C                   EVALR     dB1_QTAORD   = %trim(%editc(wNum30_0:'2'))
076500060228     C                   ELSE
076600060228     C                   Z-ADD     PiVal         wNum30_3         30 3
076700060228     C                   EVALR     dB1_QTAORD   = %trim(%editc(wNum30_3:'2'))
076800060228     C                   ENDIF
076900060228     C                   ENDIF
077000060227     C                   ENDIF
077100060227     C*
077200060227     C********
077300060227     C* Tipo record 'E1EDL41'
077400060227     C* - soto tipo '001' => Riferimenti e date ordini - cliente
077500060227     C********
077600060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL41' AND
077700060227     C                             %trim(%subst(vindta:64:3)) = '001'
077800060227     C* Valorizzazione => DS_B2
077900060227     C                   EVAL      dB2_CLIORDNUM= %trim(%subst(vindta:66+1:35))
078000060227     C                   EVAL      dB2_CLIORDDAT= %trim(%subst(vindta:101+1:8))
078100060227     C                   EVAL      dB2_CLIORDDAT= %subst(dB2_CLIORDDAT:7:2)+'.'+
078200060227     C                                            %subst(dB2_CLIORDDAT:5:2)+'.'+
078300060227     C                                            %subst(dB2_CLIORDDAT:1:4)
078400060227     C                   ENDIF
078500060227     C*
078600060227     C********
078700060227     C* Tipo record 'E1EDL41'
078800060227     C* - soto tipo '002' => Riferimenti e date ordini - evasione mittente
078900060227     C********
079000060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL41' AND
079100060227     C                             %trim(%subst(vindta:64:3)) = '002'
079200060227     C* Valorizzazione => DS_B3
079300060227     C                   EVAL      dB3_CLIORDNUM= %trim(%subst(vindta:66+1:35))
079400060227     C                   EVAL      dB3_CLIORDDAT= %trim(%subst(vindta:101+1:8))
079500060227     C                   EVAL      dB3_CLIORDDAT= %subst(dB3_CLIORDDAT:7:2)+'.'+
079600060227     C                                            %subst(dB3_CLIORDDAT:5:2)+'.'+
079700060227     C                                            %subst(dB3_CLIORDDAT:1:4)
079800060227     C                   ENDIF
079900060227     C*
080000060227     C********
080100060227     C* Tipo record 'E1EDL43'
080200060227     C* - soto tipo 'C' => IDoc Riferimenti
080300060227     C********
080400060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL43' AND
080500060227     C                             %trim(%subst(vindta:64:1)) = 'C'
080600060227     C* Valorizzazione => DS_B5
080700060228     C***                EVAL      dB5_IDOCNUM  = %trim(%subst(vindta:64+1:35))
080800060228     C                   EVAL      dB5_IDOCNUM  = %trim(%subst(vindta:64+1:10))
080900060228     C                   MOVE(P)   dB5_IDOCNUM   wNum10_0         10 0
081000060228     C                   EVALR     dB5_IDOCNUM  = %trim(%editc(wNum10_0:'4'))
081100060227     C                   ENDIF
081200060227     C*
081300060227     C********
081400060227     C* Tipo record 'E1EDL24' Informazioni elementi testata spedizione
081500060227     C********
081600060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL24'
081700060227     C* Valorizzazione => DS_B6
081800060227     C                   IF        %trim(%subst(vindta:336+1:6)) <> *blanks
081900060227     C                   EVAL      dB6_DETPOSNUM= %trim(%subst(vindta:336+1:6))
082000060227     C                   ELSE
082100060227     C                   EVAL      dB6_DETPOSNUM= %trim(%subst(vindta:63+1:6))
082200060227     C                   ENDIF
082300060228     C                   EVALR     dB6_DETMATNUM= %trim(%subst(vindta:69+1:18))
082400060227     C                   MOVE(P)   dB6_DETMATNUM wNum18_0         18 0
082500060228     C                   EVALR     dB6_DETMATNUM= %trim(%editc(wNum18_0:'4'))
082600060227     C                   EVAL      dB6_DETMATDES= %trim(%subst(vindta:105+1:40))
082700060227     C                   EVAL      dB6_DETBARCOD= %trim(%subst(vindta:207+1:10))
082800060228     C                   EVALR     dB6_DETQTASPE= %trim(%subst(vindta:239+1:15))
082900060227     C                   EVAL      PiStr=dB6_DETQTASPE
083000060227     C                   EXSR      CHKNUM
083100060228     C                   IF        PiNum=*on
083200060228     C                   IF        PiInt=*on
083300060228     C                   Z-ADD     PiVal         wNum30_0         30 0
083400060228     C                   EVALR     dB6_DETQTASPE= %trim(%editc(wNum30_0:'2'))
083500060228     C                   ELSE
083600060228     C                   Z-ADD     PiVal         wNum30_3         30 3
083700060228     C                   EVALR     dB6_DETQTASPE= %trim(%editc(wNum30_3:'2'))
083800060228     C                   ENDIF
083900060228     C                   ENDIF
084000060227     C                   EVAL      dB6_DETUNIT  = %trim(%subst(vindta:254+1:3))
084100060227     C*
084200060227     C* Trattandosi d record "ripetuto" memorizzo in specifica schiera
084300060228     C                   ADD       1             jVAR_PK
084400060228     C                   EVAL      skVAR_PKB6(jVAR_PK) = DS_B6
084500060227     C                   ENDIF
084600060228     C*
084700060228     C********
084800060228     C* Tipo record 'E1EDL15'
084900060228     C* - soto tipo '9999999400' => IDoc Caratteristiche
085000060228     C********
085100060228     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL15' AND
085200060228     C                             %trim(%subst(vindta:64:10)) = '9999999400'
085300060228     C* Valorizzazione => DS_B4
085400060228     C                   EVAL      dB4_DATSCAD  = %trim(%subst(vindta:133+1:30))
085500060228     C*
085600060228     C* Trattandosi d record "ripetuto" memorizzo in specifica schiera
085700060228     C                   EVAL      skVAR_PKB4(jVAR_PK) = DS_B4
085800060228     C                   ENDIF
085900060228     C*
086000060228     C********
086100060228     C* Tipo record 'E1EDL15'
086200060228     C* - soto tipo '0000000823' => IDoc Caratteristiche
086300060228     C********
086400060228     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL15' AND
086500060228     C                             %trim(%subst(vindta:64:10)) = '0000000823'
086600060228     C* Valorizzazione => DS_B4_1
086700060228     C                   EVAL      dB4_1_DATSCAD= %trim(%subst(vindta:133+1:30))
086800060228     C*
086900060228     C* Trattandosi d record "ripetuto" memorizzo in specifica schiera
087000060228     C                   EVAL      skVAR_PKB4_1(jVAR_PK) = DS_B4_1
087100060228     C                   ENDIF
087200060227     C*
087300041210     C********
087400050204     C* Tipo record 'E1EDL20' (dati testata: riferimenti, peso, volume, colli, etc..)
087500041210     C********
087600060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL20'
087700060227     C* Valorizzazione => DS_A8
087800060228     C                   EVALR     dA8_NUMDOC   = %trim(%subst(vindta:63+1:10))
087900060227     C                   MOVE(P)   dA8_NUMDOC    wNum10_0         10 0
088000060228     C                   EVALR     dA8_NUMDOC   = %trim(%editc(wNum10_0:'4'))
088100060227     C                   EVAL      dA8_PORTO1   = %trim(%subst(vindta:115+1:3))
088200060227     C                   EVAL      dA8_PORTO2   = %trim(%subst(vindta:118+1:28))
088300060227     C                   EVAL      dA8_ROUTE    = %trim(%subst(vindta:146+1:6))
088400060228     C                   EVALR     dA8_PESOTOT  = %trim(%subst(vindta:154+1:17))
088500060227     C                   EVAL      PiStr=dA8_PESOTOT
088600060227     C                   EXSR      CHKNUM
088700060228     C                   IF        PiNum=*on
088800060228     C                   IF        PiInt=*on
088900060228     C                   Z-ADD     PiVal         wNum30_0         30 0
089000060228     C                   EVALR     dA8_PESOTOT  = %trim(%editc(wNum30_0:'2'))
089100060228     C                   ELSE
089200060228     C                   Z-ADD     PiVal         wNum30_3         30 3
089300060228     C                   EVALR     dA8_PESOTOT  = %trim(%editc(wNum30_3:'2'))
089400060228     C                   ENDIF
089500060228     C                   ENDIF
089600060228     C                   EVALR     dA8_PESONET  = %trim(%subst(vindta:171+1:15))
089700060227     C                   EVAL      PiStr=dA8_PESONET
089800060227     C                   EXSR      CHKNUM
089900060228     C                   IF        PiNum=*on
090000060228     C                   IF        PiInt=*on
090100060228     C                   Z-ADD     PiVal         wNum30_0         30 0
090200060228     C                   EVALR     dA8_PESONET  = %trim(%editc(wNum30_0:'2'))
090300060228     C                   ELSE
090400060228     C                   Z-ADD     PiVal         wNum30_3         30 3
090500060228     C                   EVALR     dA8_PESONET  = %trim(%editc(wNum30_3:'2'))
090600060228     C                   ENDIF
090700060228     C                   ENDIF
090800060227     C                   EVAL      dA8_PESOUNIT = %trim(%subst(vindta:186+1:3))
090900060228     C                   EVALR     dA8_VOLTOT   = %trim(%subst(vindta:189+1:15))
091000060227     C                   EVAL      PiStr=dA8_VOLTOT
091100060227     C                   EXSR      CHKNUM
091200060228     C                   IF        PiNum=*on
091300060228     C                   IF        PiInt=*on
091400060228     C                   Z-ADD     PiVal         wNum30_0         30 0
091500060228     C                   EVALR     dA8_VOLTOT   = %trim(%editc(wNum30_0:'2'))
091600060228     C                   ELSE
091700060228     C                   Z-ADD     PiVal         wNum30_3         30 3
091800060228     C                   EVALR     dA8_VOLTOT   = %trim(%editc(wNum30_3:'2'))
091900060228     C                   ENDIF
092000060228     C                   ENDIF
092100060227     C                   EVAL      dA8_VOLUNIT  = %trim(%subst(vindta:204+1:3))
092200060228     C                   EVALR     dA8_COLLI    = %trim(%subst(vindta:207+1:5))
092300060227     C                   MOVE(P)   dA8_COLLI     wNum5_0           5 0
092400060228     C                   EVALR     dA8_COLLI    = %trim(%editc(wNum5_0:'2'))
092500060227     C*
092600041210     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del FIVAB
092700041210     C                   IF        FlgNewBol = '1'
092800041210     C                   WRITE     FIVAB000
092900041210     C                   IF        VATNOT <> *blanks
093000041210     C                   WRITE     FIVAT000
093100041210     C                   ENDIF
093200060227     C                   EXSR      WRIVAX
093300060227     C                   EVAL      FlgNewBol = '0'
093400041210     C                   ENDIF
093500041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
093600040910     C                   EXSR      INZVAR
093700040910     C                   EXSR      DEFCAM
093800041210     C                   EVAL      FlgNewBol = '1'
093900040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
094000040928     C                   MOVEL     datcor        VABAAS
094100040928     C                   MOVEL     datcor        VATAAS
094200060225     C                   MOVEL     datcor        VAXAAS
094300040928     C                   MOVE      datcor        VABMGS
094400040928     C                   MOVE(P)   vlrpoi        VABFGS
094500040928     C                   MOVE(P)   vlrpoi        VATFGS
094600060225     C                   MOVE(P)   vlrpoi        VAXFGS
094700060225     C* ......VABNSP/VATNSP/VAXNSP => Stacco un numeratore da AZNUM
094800041210     C                   CLEAR                   TRUL33DS
094900041210     C                   EVAL      I33OPE = *zeros
095000041210     C                   EVAL      I33CNU = 302
095100041210     C                   EVAL      I33NUM = 1
095200041210     C                   MOVEL     TRUL33DS      KPJBU
095300041210     C                   CALL      'TRUL33R'
095400041210     C                   PARM                    KPJBA
095500041210     C                   MOVEL     KPJBU         TRUL33DS
095600041210     C                   IF        O33ERR = *zeros
095700041210     C                   Z-ADD     O33NRF        VABNSP
095800041210     C                   Z-ADD     O33NRF        VATNSP
095900060225     C                   Z-ADD     O33NRF        VAXNSP
096000041210     C                   ELSE
096100041210     C                   ADD       1             errore
096200041210     C                   EVAL      vinmsg = %trimr(vinmsg)
096300060225     C                             + ' ' + 'VABNSP VATNSP VAXNSP'
096400041210     C                   ENDIF
096500041210     C* ......VABRMN
096600041210     C                   EVAL      PiStr=%trim(%subst(vindta:64:10))
096700041210     C                   EXSR      CHKNUM
096800041210     C                   IF        PiInt=*on
096900041210     C                   Z-ADD     PiVal         VABRMN
097000041210     C                   ELSE
097100041210     C                   Z-ADD     1             VABRMN
097200041210     C                   ADD       1             errore
097300041210     C                   EVAL      vinmsg = %trimr(vinmsg)
097400041210     C                             + ' ' + 'VABRMN'
097500041210     C                   ENDIF
097600041210     C* ......VABRMA
097700041210     C                   EVAL      VABRMA=%trim(%subst(vindta:64:10))
097800041210     C* ......VABPKB
097900041210     C                   EVAL      PiStr=%trim(%subst(vindta:155:17))
098000041210     C                   EXSR      CHKNUM
098100041210     C                   IF        PiNum=*on
098200041210     C                   IF        %trim(%subst(vindta:187:3)) = 'GRM'          * da grammi a kili
098300041210     C                   EVAL      PiVal=PiVal/1000
098400050209     C                   Z-ADD     PiVal         VABPKB
098500041210     C                   ELSE
098600050209     C                   Z-ADD     PiVal         VABPKB                        * altrimenti dft. kg
098700041210     C                   ENDIF
098800041210     C                   ELSE
098900041210     C                   ADD       1             errore
099000041210     C                   EVAL      vinmsg = %trimr(vinmsg)
099100041210     C                             + ' ' + 'VABPKB'
099200041210     C                   ENDIF
099300041210     C* ......VABVLB
099400041210     C                   EVAL      PiStr=%trim(%subst(vindta:190:15))
099500041210     C                   EXSR      CHKNUM
099600041210     C                   IF        PiNum=*on
099700041210     C                   IF        %trim(%subst(vindta:205:3)) = 'DMQ'          * da dec3 a m3
099800041210     C                   EVAL      PiVal=PiVal/1000
099900050209     C                   Z-ADD     PiVal         VABVLB
100000041210     C                   ELSE
100100050209     C                   Z-ADD     PiVal         VABVLB                         * altrimenti dft. m3
100200041210     C                   ENDIF
100300041210     C                   ELSE
100400041210     C                   ADD       1             errore
100500041210     C                   EVAL      vinmsg = %trimr(vinmsg)
100600041210     C                             + ' ' + 'VABVLB'
100700041210     C                   ENDIF
100800041210     C* ......VABNCL
100900041210     C                   EVAL      PiStr=%trim(%subst(vindta:208:5))
101000041210     C                   EXSR      CHKNUM
101100041210     C                   IF        PiInt=*on
101200041210     C                   Z-ADD     PiVal         VABNCL
101300041210     C                   ELSE
101400041210     C                   ADD       1             errore
101500041210     C                   EVAL      vinmsg = %trimr(vinmsg)
101600041210     C                             + ' ' + 'VABNCL'
101700041210     C                   ENDIF
101800041210     C*
101900041210     C                   ENDIF
102000041210     C*
102100041210     C********
102200050204     C* Tipo record 'E1ADRM1' (dati instradamento: rag. soc. dest, indirizzo dest, etc...)
102300041210     C* - soto tipo 'WE' => Destinatario Merce
102400041210     C********
102500050204     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM1' AND
102600041210     C                             %trim(%subst(vindta:64:3)) = 'WE'
102700060227     C* Valorizzazione => DS_A4
102800060228     C                   EVALR     dA4_DESTID   = %trim(%subst(vindta:67+1:17))
102900060227     C                   MOVE(P)   dA4_DESTID    wNum17_0         17 0
103000060228     C                   EVALR     dA4_DESTID   = %trim(%editc(wNum17_0:'4'))
103100060227     C                   EVAL      dA4_DESTRSD  = %trim(%subst(vindta:118+1:17))
103200060227     C                   EVAL      dA4_DESTRD2  = %trim(%subst(vindta:158+1:40))
103300060227     C                   EVAL      dA4_DESTIND  = %trim(%subst(vindta:438+1:40))
103400060227     C                   EVAL      dA4_DESTCAP  = %trim(%subst(vindta:572+1:10))
103500060227     C                   EVAL      dA4_DESTLOC  = %trim(%subst(vindta:607+1:40))
103600060227     C                   EVAL      dA4_DESTNAZ  = %trim(%subst(vindta:937+1:3))
103700060227     C                   EVAL      dA4_DESTPRV  = %trim(%subst(vindta:943+1:3))
103800060227     C*
103900041210     C* ......VABRSD/VABRD2
104000041210     C                   MOVEL     *blanks       wRSD             70
104100041210     C                   EVAL      wRSD = %trim(%subst(vindta:119:40)) + ' ' +
104200041210     C                                    %trim(%subst(vindta:159:40)) + ' ' +
104300041210     C                                    %trim(%subst(vindta:199:40)) + ' ' +
104400041210     C                                    %trim(%subst(vindta:239:40))
104500041210     C                   EVAL      VABRSD=%subst(wRSD:1:35)
104600041210     C                   EVAL      VABRD2=%subst(wRSD:1+35:35)
104700041210     C* ......VABIND
104800041210     C                   EVAL      VABIND=%trim(%subst(vindta:439:40)) + ' ' +
104900041210     C                                    %trim(%subst(vindta:479:40)) + ' ' +
105000041210     C                                    %trim(%subst(vindta:519:40)) + ' ' +
105100041210     C                                    %trim(%subst(vindta:559:40))
105200041210     C* ......VABCAD
105300041210     C                   EVAL      PiStr=%trim(%subst(vindta:573:10))
105400041210     C                   EXSR      CHKNUM
105500041210     C                   IF        PiInt=*on
105600041210     C                   Z-ADD     PiVal         Num5_0
105700041210     C                   MOVEL(P)  Num5_0        VABCAD
105800041210     C                   ELSE
105900041210     C                   ADD       1             errore
106000041210     C                   EVAL      vinmsg = %trimr(vinmsg)
106100041210     C                             + ' ' + 'VABCAD'
106200041210     C                   ENDIF
106300041210     C* ......VABLOD
106400041210     C                   EVAL      VABLOD=%trim(%subst(vindta:608:40)) + ' ' +
106500041210     C                                    %trim(%subst(vindta:648:40))
106600041210     C* ......VATNOT
106700041210     C                   EVAL      VATNOT=%trim(%subst(vindta:748:30)) + ' ' +
106800041210     C                                    %trim(%subst(vindta:778:40))
106900041210     C                   EVAL      VATTRC = 'B'
107000041210     C* ......VABNZD (conversone da formato *ISO a formato "BARTOLINI"
107100041210     C                   EVAL      VABNZD=%trim(%subst(vindta:938:3))
107200041210     C                   Z-ADD     1             jNAZ
107300041210     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
107400041210     C                   IF        %found
107500041210     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
107600041210     C                   ENDIF
107700060225     C* ......VABPRD
107800041210     C                   EVAL      VABPRD=%trim(%subst(vindta:944:3))
107900040910     C*
108000040910     C                   ENDIF
108100040910     C*
108200040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
108300040910     C                   IF        FlgCAS <> '0'
108400040929     C                   IF        VABCBO = '1'
108500040910     C                   EVAL      VABCBO = '4'
108600040910     C                   ELSE
108700040929     C                   EVAL      VABCBO = '6'
108800040910     C                   ENDIF
108900040929     C                   ENDIF
109000040910     C*
109100040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
109200040910     C                   EXSR      CHKIMPDIV
109300010202     C*
109400000801     C* Ebbene...
109500000801     C                   ADD       1             §CTRMO
109600010201     C                   IF        errore <> *zeros
109700000801     C                   ADD       1             §CTRNO
109800000801     C                   EVAL      recko = vindta
109900000801     C                   EXCEPT    rigadett
110000000801     C                   EVAL      vinflg = '2'
110100000801     C                   ELSE
110200010201     C                   ADD       1             §CTROKVB
110300000801     C                   ENDIF
110400000801     C*
110500000801     C                   ENDSR
110600010202     C*----------------------------------------------------*
110700060225     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR E FILE FIVAXWWR
110800010202     C*----------------------------------------------------*
110900060225     C     PREVATVAX     BEGSR
111000010202     C*
111100060225     C* Compongo il nome del membro da dare al FIVATWWR e al FIVAXWWR
111200010202     C                   eval      parmbr = vlrhdl
111300010202     C                   movel     'M'           parmbr
111400041210     C                   eval      parccm = vlrksc
111500010202     C                   eval      paropz = '1'
111600010202     C* Effettuo la chiamata al CLLE preposto
111700060225     C                   call(e)   'TITVVTXC'
111800010202     C                   parm                    parccm
111900010202     C                   parm                    parmbr
112000010202     C                   parm                    paropz
112100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
112200010202     C                   if        %error
112300010202     C                   movel     '1'           chkcall
112400010202     C                   else
112500010202     C                   movel     '0'           chkcall
112600010202     C                   endif
112700010202     C*
112800010202     C                   ENDSR
112900000801     C*----------------------------------------------------*
113000000801     C*  CONTROLLO NUMERICITA' CAMPI
113100000801     C*----------------------------------------------------*
113200000801     C     CHKNUM        BEGSR
113300000801     C*
113400000801     C                   call(e)   'ISNUMERIC'
113500000801     C                   PARM                    PiStr            30
113600041210     C                   PARM      '.'           PiDecChr          1
113700000801     C                   PARM      *ZEROS        PiVal            30 9
113800000801     C                   PARM      '0'           PiInt             1
113900000801     C                   PARM      '0'           PiNum             1
114000000801     C                   IF        %error
114100060227     C                   EVAL      PiNum=*off
114200000801     C                   ENDIF
114300000801     C*
114400000801     C                   ENDSR
114500000801     C***
114600000801
114700011113
114800011113     C*----------------------------------------------------*
114900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
115000011113     C*----------------------------------------------------*
115100011113     C     CHKIMPDIV     BEGSR
115200011113     C*
115300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
115400011113     C                   Z-ADD     *zeros        wrkDec            9 9
115500011113     C*
115600011113     C* Come prima cosa effettuo considerazioni sulla divisa
115700011113     C                   IF        vabIAS > *zeros
115800011113     C                   IF        vabVAS <> 'EUR'
115900011113     C                   EVAL      vabVAS =  'ITL'
116000011113     C                   ENDIF
116100011113     C                   ENDIF
116200011113     C*
116300011113     C                   IF        vabCAS > *zeros
116400011113     C                   IF        vabVCA <> 'EUR'
116500011113     C                   EVAL      vabVCA =  'ITL'
116600011113     C                   ENDIF
116700011113     C                   ENDIF
116800011113     C*
116900011113     C                   IF        vabVMD > *zeros
117000020305     C                   IF        vabVAD <> 'EUR'
117100011113     C                   EVAL      vabVAD =  'ITL'
117200011113     C                   ENDIF
117300011113     C                   ENDIF
117400011113     C*
117500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
117600011113     C                   Z-ADD     vabIAS        wrkDec
117700011113     C                   IF        wrkDec > *zeros
117800011113     C                   IF        vabVAS = 'ITL'
117900011113     C                   EVAL      vabIAS = *zeros
118000011113     C                   ENDIF
118100011113     C                   ENDIF
118200011113     C*
118300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
118400011113     C                   Z-ADD     vabCAS        wrkDec
118500011113     C                   IF        wrkDec > *zeros
118600011113     C                   IF        vabVCA = 'ITL'
118700011113     C                   EVAL      vabCAS = *zeros
118800011113     C                   ENDIF
118900011113     C                   ENDIF
119000011113     C*
119100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
119200011113     C                   Z-ADD     vabVMD        wrkDec
119300011113     C                   IF        wrkDec > *zeros
119400011113     C                   IF        vabVAD = 'ITL'
119500011113     C                   EVAL      vabVMD = *zeros
119600011113     C                   ENDIF
119700011113     C                   ENDIF
119800011113     C*
119900011113     C                   ENDSR
120000011113     C***
120100011113
120200060227
120300060227
120400060227     C*----------------------------------------------------*
120500060227     C*  ROUTINE GENERAZIONE BUFFER FORMATI FILE FIVAXWWR
120600060227     C*----------------------------------------------------*
120700060227     C     WRIVAX        BEGSR
120800060227     C*
120900060227     C* Inizializzo contatore d riga e flag fine dati DDT x bolla corrente
121000060227     C                   Z-ADD     1             VAXPRR
121100060228 xxx C***                MOVEL     '1'           wDDT              1
121200060228     C***                Z-ADD     *zeros        wTotVAX           5 0
121300060228     C*
121400060228     C* Valorizzo subito il totale d righe presenti nel DDT della bolla corrente
121500060228     C***                EVAL      wTotVAX = jVAR_PK + jVAR_RS + 35
121600060227     C*
121700060227     C* Imposto il tipo modulo x la stampa del logo personalizzato
121800060227     C                   EVAL      VAXTMD = 'BBRA'
121900060227     C*
122000060227     C* Ciclo o fino a fine dati memorizzati o cmq al massimo fino a 100 righe x bolla
122100060228     C***                DOW       VAXPRR <= 100 AND
122200060228     C***                          wDDT    = '1'
122300060228     C*
122400060228     C* Se esaurite le righe da stampare esco dal ciclo
122500060228     C***                IF        VAXPRR > wTotVAX
122600060228     C***                LEAVE
122700060228     C***                ENDIF
122800060227     C*
122900060227     C* Innanzitutto inizializzo il campo dati flat
123000060227     C                   CLEAR                   VAXDTT
123100060227     C*
123200060227     C* A seconda della riga corrente effettuo specifiche considerazioni
123300060228     C***                IF        VAXPRR <= 5
123400060228     C***                EVAL      VAXTRK = 'T1'
123500060228     C***                ELSE
123600060228     C***                EVAL      VAXTRK = 'C1'
123700060228     C***                ENDIF
123800060228     C*
123900060228     C* Imposto il carattere in "grassetto"
124000060228     C                   EVAL      VAXTRK = 'T1'
124100060227     C*
124200060228     C***                IF        VAXPRR = 1
124300060227     C                   RESET                   DS__T_1
124400060227     C                   EVAL      sT1_MITT = %trim(dA5_ORIGRSD)
124500060227     C                   EVAL      sT1_DEST = %trim(dA4_DESTRSD)
124600060227     C                   EVAL      VAXDTT   = DS__T_1
124700060228     C                   WRITE     FIVAX000
124800060228     C                   ADD       1             VAXPRR
124900060228     C***                ENDIF
125000060227     C*
125100060228     C***                IF        VAXPRR = 2
125200060227     C                   RESET                   DS__T_1
125300060227     C                   EVAL      sT1_MITT = %trim(dA5_ORIGIND) + ' ' +
125400060227     C                                        %trim(dA5_ORIGCIV)
125500060227     C                   EVAL      sT1_DEST = %trim(dA4_DESTRD2)
125600060227     C                   EVAL      VAXDTT   = DS__T_1
125700060228     C                   WRITE     FIVAX000
125800060228     C                   ADD       1             VAXPRR
125900060228     C***                ENDIF
126000060227     C*
126100060228     C***                IF        VAXPRR = 3
126200060227     C                   RESET                   DS__T_1
126300060227     C                   EVAL      sT1_MITT = %trim(dA5_ORIGCAP) + ' ' +
126400060227     C                                        %trim(dA5_ORIGLOC)
126500060227     C                   EVAL      sT1_DEST = %trim(dA4_DESTIND)
126600060227     C                   EVAL      VAXDTT   = DS__T_1
126700060228     C                   WRITE     FIVAX000
126800060228     C                   ADD       1             VAXPRR
126900060228     C***                ENDIF
127000060227     C*
127100060228     C***                IF        VAXPRR = 4
127200060227     C                   RESET                   DS__T_1
127300060227     C                   EVAL      sT1_MITT = %trim(dA5_ORIGNAZD)
127400060227     C                   EVAL      sT1_DEST = %trim(dA4_DESTNAZ) + '-'  +
127500060227     C                                        %trim(dA4_DESTCAP) + ' '  +
127600060227     C                                        %trim(dA4_DESTLOC) + ' ' +
127700060227     C                                        '(' + %trim(dA4_DESTPRV) + ')'
127800060227     C                   EVAL      VAXDTT   = DS__T_1
127900060228     C                   WRITE     FIVAX000
128000060228     C                   ADD       1             VAXPRR
128100060228     C***                ENDIF
128200060227     C*
128300060228     C* Imposto il carattere in "normale"
128400060228     C                   EVAL      VAXTRK = 'C1'
128500060228     C*
128600060228     C***                IF        VAXPRR = 5
128700060227     C                   EVAL      VAXDTT   = *blanks
128800060228     C                   WRITE     FIVAX000
128900060228     C                   ADD       1             VAXPRR
129000060228     C***                ENDIF
129100060227     C*
129200060228     C***                IF        VAXPRR = 6
129300060227     C                   RESET                   DS__T_2
129400060227     C                   EVAL      sT2_TES1_1 = 'Nr.Documento:'
129500060227     C                   EVAL      sT2_TES1_2 = %trim(dA8_NUMDOC)
129600060227     C                   EVAL      sT2_TES2_1 = 'Nr.Ordine vendita:'
129700060227     C                   EVAL      sT2_TES2_2 = %trim(dA1_NUMORD)
129800060227     C                   EVAL      VAXDTT   = DS__T_2
129900060228     C                   WRITE     FIVAX000
130000060228     C                   ADD       1             VAXPRR
130100060228     C***                ENDIF
130200060227     C*
130300060228     C***                IF        VAXPRR = 7
130400060227     C                   RESET                   DS__T_2
130500060227     C                   EVAL      sT2_TES1_1 = 'Data spedizione:'
130600060227     C                   EVAL      sT2_TES1_2 = %trim(dA6_DATSPE)
130700060227     C                   EVAL      sT2_TES2_1 = 'Codice BBMI:'
130800060227     C                   EVAL      sT2_TES2_2 = %trim(dA4_DESTID)
130900060227     C                   EVAL      VAXDTT   = DS__T_2
131000060228     C                   WRITE     FIVAX000
131100060228     C                   ADD       1             VAXPRR
131200060228     C***                ENDIF
131300060227     C*
131400060228     C***                IF        VAXPRR = 8
131500060227     C                   RESET                   DS__T_2
131600060227     C                   EVAL      sT2_TES1_1 = 'Itinerario:'
131700060227     C                   EVAL      sT2_TES1_2 = %trim(dA8_ROUTE)
131800060227     C                   EVAL      sT2_TES2_1 = 'Committente:'
131900060227     C                   EVAL      sT2_TES2_2 = %trim(dA3_MITTID)
132000060227     C                   EVAL      VAXDTT   = DS__T_2
132100060228     C                   WRITE     FIVAX000
132200060228     C                   ADD       1             VAXPRR
132300060228     C***                ENDIF
132400060227     C*
132500060228     C***                IF        VAXPRR = 9
132600060227     C                   RESET                   DS__T_2
132700060227     C                   EVAL      sT2_TES1_1 = 'Invio:'
132800060227     C                   EVAL      sT2_TES2_1 = 'Nr.Ord. acq. com.:'
132900060227     C                   EVAL      sT2_TES2_2 = %trim(dB2_CLIORDNUM)
133000060227     C                   EVAL      VAXDTT   = DS__T_2
133100060228     C                   WRITE     FIVAX000
133200060228     C                   ADD       1             VAXPRR
133300060228     C***                ENDIF
133400060227     C*
133500060228     C***                IF        VAXPRR = 10
133600060227     C                   RESET                   DS__T_2
133700060227     C                   EVAL      sT2_TES1_1 = 'Tipo porto:'
133800060227     C                   EVAL      sT2_TES1_2 = %trim(dA8_PORTO1) + ' '+
133900060227     C                                          %trim(dA8_PORTO2)
134000060227     C                   EVAL      sT2_TES2_1 = 'Data Ord.acq.com.:'
134100060228     C                   EVAL      sT2_TES2_2 = %trim(dB2_CLIORDDAT)
134200060227     C                   EVAL      VAXDTT   = DS__T_2
134300060228     C                   WRITE     FIVAX000
134400060228     C                   ADD       1             VAXPRR
134500060228     C***                ENDIF
134600060227     C*
134700060228     C***                IF        VAXPRR = 11
134800060227     C                   RESET                   DS__T_2
134900060227     C                   EVAL      sT2_TES2_1 = 'Referente:'
135000060227     C                   EVAL      sT2_TES2_2 = %trim(dA2_CONTAT)
135100060227     C                   EVAL      VAXDTT   = DS__T_2
135200060228     C                   WRITE     FIVAX000
135300060228     C                   ADD       1             VAXPRR
135400060228     C***                ENDIF
135500060227     C*
135600060228     C***                IF        VAXPRR = 12
135700060227     C                   RESET                   DS__T_2
135800060227     C                   EVAL      sT2_TES2_1 = 'Tel Ordine proc.:'
135900060227     C                   EVAL      sT2_TES2_2 = %trim(dA2_TEL)
136000060227     C                   EVAL      VAXDTT   = DS__T_2
136100060228     C                   WRITE     FIVAX000
136200060228     C                   ADD       1             VAXPRR
136300060228     C***                ENDIF
136400060227     C*
136500060228     C***                IF        VAXPRR = 13
136600060227     C                   RESET                   DS__T_2
136700060227     C                   EVAL      sT2_TES2_1 = 'Fax Ordine proc.:'
136800060227     C                   EVAL      sT2_TES2_2 = %trim(dA2_FAX)
136900060227     C                   EVAL      VAXDTT   = DS__T_2
137000060228     C                   WRITE     FIVAX000
137100060228     C                   ADD       1             VAXPRR
137200060228     C***                ENDIF
137300060227     C*
137400060228     C***                IF        VAXPRR = 14
137500060227     C                   EVAL      VAXDTT   = *blanks
137600060228     C                   WRITE     FIVAX000
137700060228     C                   ADD       1             VAXPRR
137800060228     C***                ENDIF
137900060227     C*
138000060228     C***                IF        VAXPRR = 15
138100060227     C                   RESET                   DS__D_1
138200060227     C                   EVAL      VAXDTT   = DS__D_1
138300060228     C                   WRITE     FIVAX000
138400060228     C                   ADD       1             VAXPRR
138500060228     C***                ENDIF
138600060227     C*
138700060228     C***                IF        VAXPRR = 16
138800060227     C                   RESET                   DS__D_2
138900060227     C                   EVAL      VAXDTT   = DS__D_2
139000060228     C                   WRITE     FIVAX000
139100060228     C                   ADD       1             VAXPRR
139200060228     C***                ENDIF
139300060228     C*
139400060228     C***                IF        VAXPRR = 17
139500060228     C                   RESET                   DS__R_1
139600060228     C                   EVAL      VAXDTT   = DS__R_1
139700060228     C                   WRITE     FIVAX000
139800060228     C                   ADD       1             VAXPRR
139900060228     C***                ENDIF
140000060227     C*
140100060228     C***                IF        VAXPRR = 18
140200060227     C                   RESET                   DS__D_3
140300060227     C                   EVAL      VAXDTT   = DS__D_3
140400060228     C                   WRITE     FIVAX000
140500060228     C                   ADD       1             VAXPRR
140600060228     C***                ENDIF
140700060227     C*
140800060228     C***                IF        VAXPRR = 19
140900060227     C                   RESET                   DS__D_4
141000060227     C                   EVAL      sD4_MITT   = %trim(dA3_MITTRSD)
141100060227     C                   EVAL      sD4_VETT   = %trim(dA7_VETTRSD)
141200060227     C                   EVAL      VAXDTT   = DS__D_4
141300060228     C                   WRITE     FIVAX000
141400060228     C                   ADD       1             VAXPRR
141500060228     C***                ENDIF
141600060227     C*
141700060228     C***                IF        VAXPRR = 20
141800060227     C                   RESET                   DS__D_4
141900060227     C                   EVAL      sD4_MITT   = %trim(dA3_MITTIND)
142000060227     C                   EVAL      sD4_VETT   = %trim(dA7_VETTIND)
142100060227     C                   EVAL      VAXDTT   = DS__D_4
142200060228     C                   WRITE     FIVAX000
142300060228     C                   ADD       1             VAXPRR
142400060228     C***                ENDIF
142500060227     C*
142600060228     C***                IF        VAXPRR = 21
142700060227     C                   RESET                   DS__D_4
142800060227     C                   EVAL      sD4_MITT   = %trim(dA3_MITTCAP) + ' ' +
142900060227     C                                          %trim(dA3_MITTLOC)
143000060227     C                   EVAL      sD4_VETT   = %trim(dA7_VETTCAP) + ' ' +
143100060227     C                                          %trim(dA7_VETTLOC)
143200060227     C                   EVAL      VAXDTT   = DS__D_4
143300060228     C                   WRITE     FIVAX000
143400060228     C                   ADD       1             VAXPRR
143500060228     C***                ENDIF
143600060227     C*
143700060228     C***                IF        VAXPRR = 22
143800060227     C                   EVAL      VAXDTT   = *blanks
143900060228     C                   WRITE     FIVAX000
144000060228     C                   ADD       1             VAXPRR
144100060228     C***                ENDIF
144200060227     C*
144300060228     C***                IF        VAXPRR = 23
144400060227     C                   RESET                   DS__D_5
144500060227     C                   EVAL      sD5_DESCR  = 'Le seguenti posizioni '       +
144600060228     C                                      'articolo/i con Nr.ord. '          +
144700060227     C                                      %trim(dB5_IDOCNUM) + ', '          +
144800060228     C                                      'Nr.ord. acquisto '                +
144900060227     C                                      %trim(dB2_CLIORDNUM) + ', '        +
145000060228     C                                      'Data ord. acquisto '              +
145100060227     C                                      %trim(dB2_CLIORDDAT)
145200060228     C                   EVAL      VAXDTT   = DS__D_5
145300060228     C                   WRITE     FIVAX000
145400060228     C                   ADD       1             VAXPRR
145500060228     C***                ENDIF
145600060227     C*
145700060228     C***                IF        VAXPRR = 24
145800060227     C                   RESET                   DS__D_6
145900060227     C                   EVAL      sD6_POSNUM = %trim(dB6_DETPOSNUM)
146000060227     C                   EVAL      sD6_MATNUM = %trim(dB6_DETMATNUM)
146100060227     C                   EVAL      sD6_QTA    = '???'
146200060228     C                   EVAL      sD6_QTAORD = %trim(dB1_QTAORD) + ' ' +
146300060228     C                                          %trim(dB1_UNIT)
146400060228     C                   EVAL      sD6_QTARES = %trim(dB1_QTADIF) + ' ' +
146500060228     C                                          %trim(dB1_UNIT)
146600060227     C                   EVAL      VAXDTT   = DS__D_6
146700060228     C                   WRITE     FIVAX000
146800060228     C                   ADD       1             VAXPRR
146900060228     C***                ENDIF
147000060228     C*
147100060228     C* Se presenti informazioni sui dettagli Packing List => gestisco....
147200060228     C                   IF        jVAR_PK > *zeros
147300060228     C* ...descrizione (riga 25)
147400060228     C                   RESET                   DS__D_7
147500060228     C                   EVAL      sD7_DETMATDES=%trim(dB6_DETMATDES)
147600060228     C                   EVAL      VAXDTT   = DS__D_7
147700060228     C                   WRITE     FIVAX000
147800060228     C                   ADD       1             VAXPRR
147900060228     C* ...riferimenti ordine cliente (riga 26)
148000060228     C                   RESET                   DS__D_8
148100060228     C                   EVAL      sD8_CLIORDNUM=%trim(dB3_CLIORDNUM)
148200060228     C                   EVAL      sD8_CLIORDDAT=%trim(dB3_CLIORDDAT)
148300060228     C                   EVAL      VAXDTT   = DS__D_8
148400060228     C                   WRITE     FIVAX000
148500060228     C                   ADD       1             VAXPRR
148600060228     C* ...ciclo su dettaglio (<n> righe)
148700060228     C                   Z-ADD     *zeros        jWRK              3 0
148800060228     C                   DO        *hival
148900060228     C*
149000060228     C* Innanzitutto testo il contatore righe stampate e se > 100 => esco dal ciclo....
149100060228     C                   IF        VAXPRR > 100
149200060228     C                   LEAVE
149300060228     C                   ENDIF
149400060228     C*
149500060228     C* Incremento l'indice della schiera corrente
149600060228     C                   ADD       1             jWRK
149700060228     C*
149800060228     C* Finchè memorizzato qualcosa...  altrimenti esco dal ciclo
149900060228     C                   IF        skVAR_PKB6(jWRK) <> *blanks
150000060228     C                   IF        skVAR_PKB4_1(jWRK) <> *blanks
150100060228     C                   EVAL      wDS_B4 = skVAR_PKB4_1(jWRK)
150200060228     C                   ELSE
150300060228     C                   EVAL      wDS_B4 = skVAR_PKB4(jWRK)
150400060228     C                   ENDIF
150500060228     C                   EVAL      wDS_B6 = skVAR_PKB6(jWRK)
150600060228     C                   RESET                   DS__D_9
150700060228     C                   EVAL      sD9_DETBARCOD=%trim(wdB6_DETBARCOD)
150800060228     C                   EVAL      sD9_DATSCAD  =%trim(wdB4_DATSCAD)
150900060228     C                   EVAL      VAXDTT   = DS__D_9
151000060228     C*
151100060228     C* Quindi scarico il buffer della riga corrente del DDT bolla corrente
151200060228     C                   WRITE     FIVAX000
151300060228     C*
151400060228     C* Incremento il contatore d riga corrente
151500060228     C                   ADD       1             VAXPRR
151600060228     C*
151700060228     C                   ELSE
151800060228     C*
151900060228     C* Al primo elemento d schiera nn valorizzato => esco dal ciclo...
152000060228     C                   LEAVE
152100060228     C                   ENDIF
152200060228     C*
152300060228     C                   ENDDO
152400060228     C                   ENDIF
152500060228     C*
152600060228     C***                IF        VAXPRR = 25
152700060228     C                   EVAL      VAXDTT   = *blanks
152800060228     C                   WRITE     FIVAX000
152900060228     C                   ADD       1             VAXPRR
153000060228     C***                ENDIF
153100060228     C*
153200060228     C***                IF        VAXPRR = 26
153300060227     C*
153400060227     C* Se presenti informazioni sul "Residuo da evadere" => gestisco....
153500060227     C                   IF        jVAR_RS > *zeros
153600060228     C* ...titolo  (riga 26)
153700060227     C                   RESET                   DS__E_1
153800060227     C                   EVAL      VAXDTT   = DS__E_1
153900060228     C                   EVAL      VAXTRK = 'T1'                                * vado in grassetto
154000060227     C                   WRITE     FIVAX000
154100060228     C                   ADD       1             VAXPRR
154200060228     C                   EVAL      VAXTRK = 'C1'                                * torno a normale
154300060228     C* ...intestazioni colonne (riga 27)
154400060227     C                   RESET                   DS__E_2
154500060227     C                   EVAL      VAXDTT   = DS__E_2
154600060227     C                   WRITE     FIVAX000
154700060228     C                   ADD       1             VAXPRR
154800060228     C* ...riga separazione "alta" (riga 28)
154900060228     C                   RESET                   DS__R_1
155000060228     C                   EVAL      VAXDTT   = DS__R_1
155100060227     C                   WRITE     FIVAX000
155200060228     C                   ADD       1             VAXPRR
155300060228     C* ...ciclo su dettaglio (<n> righe)
155400060228     C                   Z-ADD     *zeros        jWRK              3 0
155500060228     C                   DO        *hival
155600060228     C*
155700060228     C* Incremento l'indice della schiera corrente
155800060228     C                   ADD       1             jWRK
155900060228     C*
156000060228     C* Finchè memorizzato qualcosa...  altrimenti esco dal ciclo
156100060228     C                   IF        skVAR_RS(jWRK) <> *blanks
156200060228     C                   EVAL      wDS_A1 = skVAR_RS(jWRK)
156300060228     C                   RESET                   DS__E_3
156400060228     C                   EVAL      sE3_NUMORD = %trim(wdA1_NUMORD)
156500060228     C                   EVAL      sE3_POS    = %trim(wdA1_POS)
156600060228     C                   EVAL      sE3_MATNUM = %trim(wdA1_MATNUM)
156700060228     C                   EVAL      sE3_MATDES = %trim(wdA1_MATDES)
156800060228     C                   EVAL      sE3_QTAORD = %trim(wdA1_QTAORD) + ' ' +
156900060228     C                                          %trim(wdA1_UNIT)
157000060228     C                   EVAL      sE3_QTADIF = %trim(wdA1_QTADIF) + ' ' +
157100060228     C                                          %trim(wdA1_UNIT)
157200060228     C                   EVAL      VAXDTT   = DS__E_3
157300060228     C*
157400060228     C* Quindi scarico il buffer della riga corrente del DDT bolla corrente
157500060228     C                   WRITE     FIVAX000
157600060228     C*
157700060228     C* Incremento il contatore d riga corrente
157800060228     C                   ADD       1             VAXPRR
157900060228     C*
158000060228     C                   ELSE
158100060228     C*
158200060228     C* Al primo elemento d schiera nn valorizzato => esco dal ciclo...
158300060228     C                   LEAVE
158400060228     C                   ENDIF
158500060227     C*
158600060227     C                   ENDDO
158700060228     C                   ENDIF
158800060228     C***                ENDIF
158900060228 vvv C*
159000060228     C***                IF        VAXPRR = 36
159100060228     C                   RESET                   DS__R_1
159200060228     C                   EVAL      VAXDTT   = DS__R_1
159300060228     C                   WRITE     FIVAX000
159400060228     C                   ADD       1             VAXPRR
159500060228     C***                ENDIF
159600060228     C*
159700060228     C***                IF        VAXPRR = 37
159800060228     C*                  RESET                   DS__R_2
159900060228     C*                  EVAL      VAXDTT   = DS__R_2
160000060228     C*                  WRITE     FIVAX000
160100060228     C*                  ADD       1             VAXPRR
160200060228     C***                ENDIF
160300060228     C*
160400060228     C***                IF        VAXPRR = 38
160500060228     C                   RESET                   DS__P_1
160600060228     C                   EVAL      sP1_DICITUR1='Peso totale:'
160700060228     C                   EVAL      sP1_VALORE1 =%trim(dA8_PESOTOT) + ' ' +
160800060228     C                                          %trim(dA8_PESOUNIT)
160900060228     C                   EVAL      sP1_DICITUR2='Numero colli:'
161000060228     C                   EVAL      sP1_VALORE2 =%trim(dA8_COLLI)
161100060228     C                   EVAL      VAXDTT   = DS__P_1
161200060228     C                   WRITE     FIVAX000
161300060228     C                   ADD       1             VAXPRR
161400060228     C***                ENDIF
161500060228     C*
161600060228     C***                IF        VAXPRR = 39
161700060228     C                   RESET                   DS__P_1
161800060228     C                   EVAL      sP1_DICITUR1='Peso netto:'
161900060228     C                   EVAL      sP1_VALORE1 =%trim(dA8_PESONET) + ' ' +
162000060228     C                                          %trim(dA8_PESOUNIT)
162100060228     C                   EVAL      VAXDTT   = DS__P_1
162200060228     C                   WRITE     FIVAX000
162300060228     C                   ADD       1             VAXPRR
162400060228     C***                ENDIF
162500060228     C*
162600060228     C***                IF        VAXPRR = 40
162700060228     C                   RESET                   DS__P_1
162800060228     C                   EVAL      sP1_DICITUR1='Volume totale:'
162900060228     C                   EVAL      sP1_VALORE1 =%trim(dA8_VOLTOT) + ' ' +
163000060228     C                                          %trim(dA8_VOLUNIT)
163100060228     C                   EVAL      sP1_DICITUR2='Conferma ricevimento (Data e '+
163200060228     C                                          'firma):'
163300060228     C                   EVAL      VAXDTT   = DS__P_1
163400060228     C                   WRITE     FIVAX000
163500060228     C                   ADD       1             VAXPRR
163600060228     C***                ENDIF
163700060228     C*
163800060228     C***                IF        VAXPRR = 41
163900060228     C                   RESET                   DS__R_1
164000060228     C                   EVAL      VAXDTT   = DS__R_1
164100060228     C                   WRITE     FIVAX000
164200060228     C                   ADD       1             VAXPRR
164300060228     C***                ENDIF
164400060227     C*
164500060227     C* Quindi scarico il buffer della riga corrente del DDT bolla corrente
164600060228     C***                WRITE     FIVAX000
164700060227     C*
164800060227     C* Incremento il contatore d riga corrente
164900060228     C***                ADD       1             VAXPRR
165000060227     C*
165100060227     C* Proseguo con la generazione dei dati x stampa DDT
165200060228     C***                ENDDO
165300060227     C*
165400060227     C                   ENDSR
165500011113
165600000801
165700000801
165800990920      /TITLE Invio dei dati al punto operativo.
165900010202     C     invio         BEGSR
166000060225     C*
166100060225     C* 1° invio FIVAX
166200060225     C                   reset                   dscmz
166300060225     C                   move      vlrpoi        cmzdst
166400060225     C                   eval      cmzfld = 'FIVAXWWR'
166500060225     C                   eval      cmzmbd = vlrhdl
166600060225     C                   eval      %subst(cmzmbd:1:1) = 'M'
166700060225     C***                if        prmfir = *blanks
166800060225     C                   eval      cmzfla = 'FIVAX00F'
166900060225     C                   eval      cmzmba = 'FIVAX00F'
167000060225     C***                else
167100060225     C***                eval      cmzfla = prmfir
167200060225     C***                eval      cmzmba = prmfir
167300060225     C***                endif
167400060225     C                   eval      cmznrr = *zeros
167500060227     C                   move      §ctrokvx      cmznrr
167600060225     C                   eval      cmzlba = vlrfl1
167700060225     C                   call(e)   'TIS711C'
167800060225     C                   parm                    dscmz
167900060225     C                   parm      *blanks       esito
168000060225     C                   if        %error
168100060225     C                             or cmzerr = '1'
168200060225     C                             or esito  = '1'
168300060225     C                   eval      wrkesito = '3'
168400060225     C                   else
168500990920     C*
168600060225     C* 2° invio FIVAT
168700010201     C                   reset                   dscmz
168800010201     C                   move      vlrpoi        cmzdst
168900021113     C                   eval      cmzfld = 'FIVATWWR'
169000010201     C                   eval      cmzmbd = vlrhdl
169100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
169200021009     C***                if        prmfir = *blanks
169300021113     C                   eval      cmzfla = 'FIVAT00F'
169400021113     C                   eval      cmzmba = 'FIVAT00F'
169500021009     C***                else
169600021009     C***                eval      cmzfla = prmfir
169700021009     C***                eval      cmzmba = prmfir
169800021009     C***                endif
169900010201     C                   eval      cmznrr = *zeros
170000020305     C                   move      §ctrokvt      cmznrr
170100021018     C                   eval      cmzlba = vlrfl1
170200010201     C                   call(e)   'TIS711C'
170300010201     C                   parm                    dscmz
170400010201     C                   parm      *blanks       esito
170500010205     C                   if        %error
170600010205     C                             or cmzerr = '1'
170700010205     C                             or esito  = '1'
170800010205     C                   eval      wrkesito = '3'
170900010205     C                   else
171000010201     C*
171100060225     C* 3° invio FIVAB
171200010201     C                   reset                   dscmz
171300010201     C                   move      vlrpoi        cmzdst
171400010201     C                   eval      cmzfld = vlrfou
171500010201     C                   eval      cmzmbd = vlrhdl
171600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
171700021009     C***                if        prmfir = *blanks
171800021113     C                   eval      cmzfla = 'FIVAB00F'
171900021113     C                   eval      cmzmba = 'FIVAB00F'
172000021009     C***                else
172100021009     C***                eval      cmzfla = prmfir
172200021009     C***                eval      cmzmba = prmfir
172300021009     C***                endif
172400010201     C                   eval      cmznrr = *zeros
172500010201     C                   move      §ctrokvb      cmznrr
172600021018     C                   eval      cmzlba = vlrfl1
172700010201     C                   call(e)   'TIS711C'
172800010201     C                   parm                    dscmz
172900010201     C                   parm      *blanks       esito
173000010201     C                   if        %error
173100010201     C                             or cmzerr = '1'
173200010201     C                             or esito  = '1'
173300010201     C                   eval      wrkesito = '3'
173400010201     C                   endif
173500010205     C                   endif
173600060225     C                   endif
173700990920     C*
173800000613     C                   ENDSR
173900000613     C***
174000990910
174100000613     C     *inzsr        BEGSR
174200990910     C*
174300990910     C     *entry        plist
174400990920     C                   parm                    tivlrds
174500990921     C                   parm      wrkesito      esito
174600000724     C                   parm                    prmlit
174700000710     C                   parm                    prmfir
174800000613     C*
174900000830     C* CALCOLA LA DATA CORRENTE
175000000830     C                   time                    wn14             14 0
175100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
175200000830     C                   z-add     wn8           g08dat
175300000830     C                   z-add     *zeros        g08inv
175400000830     C                   movel     '0'           g08err
175500000830     C                   call      'XSRDA8'
175600000830     C                   parm                    wlbda8
175700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
175800041210     C*
175900041210     C* Chiave su TABEL00F - parziale
176000041210     C     KEYtabP       KLIST
176100041210     C                   KFLD                    tblKUT
176200041210     C                   KFLD                    tblCOD
176300000830     C*
176400000613     C                   ENDSR
176500000613     C***
176600990908
176700041206     Otitvg4p   E            riepilogo         2
176800990915     O                                              'Upload via Internet'
176900990915     O                                           +1 'Traduzione TIVIN00R -'
177000060225     O                                           +1 'FIVAB/FIVAT/FIVAX'
177100010201     O                                           +1 'Report testate bolle'
177200990915     O          E            riepilogo   2
177300990915     O                       wrkdata
177400990915     O                       wrkora              +1
177500990915     O                       procname            +1
177600990915     O          E            riepilogo   2
177700990915     O                                              'Cliente..................:'
177800990915     O                       VABCCM        z     +1
177900990915     O          E            riepilogo   2
178000990920     O                                              'Riferimento Strategi.....:'
178100990920     O                       vlrhdl              +1
178200990915     O          E            riepilogo   2
178300990915     O                                              'Giusti...................:'
178400010201     O                       §CTROKVB      2   +  1
178500990915     O          E            riepilogo   2
178600990915     O                                              'Sbagliati e corretti.....:'
178700971022     O                       §CTRMO        2   +  1
178800990915     O          E            riepilogo   2
178900990915     O                                              'Sbagliati e scartati.....:'
179000971022     O                       §CTRNO        2   +  1
179100000613
179200041206     Otitvg4ps  E            testdett          2
179300000613     O                                              'Upload via Internet'
179400000613     O                                           +1 'Traduzione TIVIN00R -'
179500060225     O                                           +1 'FIVAB/FIVAT/FIVAX'
179600010201     O                                           +1 'Report testate bolle'
179700000616     O          E            testdett    3
179800000613     O                                           +2 'N° rec'
179900000613     O                                           +3 'Anteprima contenuto'
180000000616     O          E            rigadett    2
180100000613     O                       rrnum               +2
180200000621     O                       recko               +3
180300000616     O          E            findett     2
180400000613     O                       wrkdata
180500000613     O                       wrkora              +1
180600000613     O                       procname            +1
180700000616     O          E            findett     2
180800000613     O                                              'Cliente..................:'
180900000613     O                       VABCCM        z     +1
181000000616     O          E            findett     2
181100000613     O                                              'Riferimento Strategi.....:'
181200000613     O                       vlrhdl              +1
181300000616     O          E            findett     2
181400000613     O                                              'Giusti...................:'
181500010201     O                       §CTROKVB      2   +  1
181600000616     O          E            findett     2
181700000613     O                                              'Sbagliati e corretti.....:'
181800000613     O                       §CTRMO        2   +  1
181900000616     O          E            findett     2
182000000613     O                                              'Sbagliati e scartati.....:'
182100000613     O                       §CTRNO        2   +  1
182200000512** CMD - COMANDI CL
182300041206OVRPRTF FILE(TITVG4P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
182400041206OVRPRTF FILE(TITVG4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
182500041206DLTOVR FILE(TITVG4P TITVG4PS) LVL(*)
182600000512
182700000512
