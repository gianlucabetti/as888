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
004900060313     D VATTRC_E        s                   LIKE(VATTRC) INZ(*blanks)
005000060313     D VATNOT_E        s                   LIKE(VATNOT) INZ(*blanks)
005100060313     D VATTRC_B        s                   LIKE(VATTRC) INZ(*blanks)
005200060313     D VATNOT_B        s                   LIKE(VATNOT) INZ(*blanks)
005300000830
005400000830     D*------------------
005500000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
005600000830     D*------------------
005700000830     D WLBDA8          DS                  INZ
005800000830     D  G08DAT                 1      8  0
005900000830     D  G08INV                 9     16  0
006000000830     D  G08ERR                17     17
006100000830     D  G08TGI                18     22  0
006200041025     D*------------------
006300041025     D* DS REPERIMENTO NUMERATORE
006400041025     D*------------------
006500041025     D trul33ds      e ds                  inz
006600041025     D*------------------
006700041025     D* DS ARCHITETTURA
006800041025     D*------------------
006900041025     D kpjba         e ds                  inz
007000041025     D*------------------
007100060225
007200060225     D*------------------
007300060225     D* DEFINIZIONE DS D WRK X CONFORMAZIONE LAYOUT PACKING LIST
007400060225     D*------------------
007500060227     D DS__T_1         DS                  INZ
007600060225     D  sT1_LOGO                     30
007700060225     D  sT1_MITT                     25
007800060225     D  sT1_BLK1                      5
007900060225     D  sT1_DEST                     55
008000060225     D*
008100060227     D DS__T_2         DS                  INZ
008200060313     D  sT2_TES1_1                   18
008300060313     D  sT2_TES1_2                   25
008400060225     D  sT2_BLK1                      5
008500060227     D  sT2_TES2_1                   20
008600060313     D  sT2_TES2_2                   50
008700060227     D*
008800060227     D DS__R_1         DS                  INZ
008900060227     D  sR1_SEP                     116    INZ(*all'_')
009000060227     D*
009100060227     D DS__R_2         DS                  INZ
009200060227     D  sR2_SEP                     116    INZ(*all'¯')
009300060227     D*
009400060227     D DS__R_3         DS                  INZ
009500060227     D  sR3_SEP                     116    INZ(*all'=')
009600060227     D*
009700060227     D DS__D_1         DS                  INZ
009800060227     D  sD1_CONST1                   16    INZ('Posizione')
009900060313     D  sD1_CONST2                   59    INZ('Codice materiale/Descrizione')
010000060313     D  sD1_CONST3                   12    INZ('Qtà spedita')
010100060313     D  sD1_CONST4                    3    INZ
010200060313     D  sD1_CONST5                   12    INZ('Qtà ordinata')
010300060313     D  sD1_CONST6                    3    INZ
010400060313     D  sD1_CONST7                   12    INZ('Qtà residua')
010500060227     D*
010600060313     D DS__D_3         DS                  INZ
010700060313     D**sD3_DESCR                   116    INZ('Spedizione effettuata da:')
010800060313     D  sD3_DESCR                   116    INZ('Merce a vettore:')
010900060227     D*
011000060227     D DS__D_5         DS                  INZ
011100060228     D  sD5_DESCR                   116
011200060227     D*
011300060227     D DS__D_6         DS                  INZ
011400060227     D  sD6_POSNUM                   16
011500060313     D  sD6_MATNUM                   59
011600060313     D  sD6_QTA                      12
011700060313     D  sD6_BLK1                      3
011800060228     D  sD6_QTAORD                   12
011900060313     D  sD6_BLK2                      3
012000060228     D  sD6_QTARES                   12
012100060228     D*
012200060228     D DS__D_7         DS                  INZ
012300060228     D  sD7_BLK1                     16
012400060228     D  sD7_DETMATDES                40
012500060228     D*
012600060228     D DS__D_8         DS                  INZ
012700060228     D  sD8_BLK1                     16
012800060228     D  sD8_CONST1                   20    INZ('Nr. ordine cliente:')
012900060313     D* sD8_CLIORDNUM                35
013000060313     D  sD8_CLIORDNUM                20
013100060313     D  sD8_BLK2                      2
013200060228     D  sD8_CONST2                    5    INZ('del:')
013300060228     D  sD8_CLIORDDAT                10
013400060228     D*
013500060228     D DS__D_9         DS                  INZ
013600060228     D  sD9_BLK1                     16
013700060228     D  sD9_CONST1                   10    INZ('N. part:')
013800060228     D  sD9_DETBARCOD                10
013900060228     D  sD9_BLK2                      5
014000060228     D  sD9_CONST2                   12    INZ('Data scad:')
014100060313     D* sD9_DATSCAD                  30
014200060313     D  sD9_DATSCAD                  15
014300060313     D  sD9_BLK3                      7
014400060313     D* sD9_CONST3                    5    INZ('Qtà:')
014500060313     D  sD9_QTASPED                  20
014600060227     D*
014700060227     D DS__E_1         DS                  INZ
014800060227     D  sE1_CONST1                  100    INZ('INFORMAZIONI SU RESIDUO DA EVAD-
014900060228     D                                     ERE')
015000060227     D*
015100060227     D DS__E_2         DS                  INZ
015200060227     D  sE2_CONST1                   12    INZ('Nr.Doc. ')
015300060227     D  sE2_CONST2                    7    INZ('Pos.')
015400060227     D  sE2_CONST3                   20    INZ('Materiale')
015500060227     D  sE2_CONST4                   42    INZ('Descrizione')
015600060227     D  sE2_CONST5                   17    INZ('Qtà Ordinata')
015700060227     D  sE2_CONST6                   17    INZ('Qtà Residua')
015800060227     D*
015900060227     D DS__E_3         DS                  INZ
016000060227     D  sE3_NUMORD                   12
016100060227     D  sE3_POS                       7
016200060227     D  sE3_MATNUM                   20
016300060227     D  sE3_MATDES                   42
016400060227     D  sE3_QTAORD                   17
016500060227     D  sE3_QTADIF                   17
016600060228     D*
016700060228     D DS__P_1         DS                  INZ
016800060228     D  sP1_DICITUR1                 15
016900060313     D  sP1_VALORE1                  12
017000060313     D  sP1_BLK1                      4
017100060313     D  sP1_DICITUR2                 15
017200060313     D  sP1_VALORE2                  12
017300060313     D  sP1_BLK2                      4
017400060313     D  sP1_DICITUR3                 15
017500060313     D  sP1_VALORE3                  12
017600060313     D  sP1_BLK3                      4
017700060313     D  sP1_DICITUR4                 15
017800060313     D  sP1_VALORE4                   6
017900060227     D*
018000060225
018100060225     D*------------------
018200060225     D* DEFINIZIONE DS D WRK X RIDEFINIZIONE TIPI RECORD FILE CLIENTE
018300060225     D*------------------
018400060225     D DS_A1           DS                  INZ
018500060225     D  dA1_NUMORD                   10
018600060225     D  dA1_POS                       6
018700060225     D  dA1_MATNUM                   18
018800060225     D  dA1_MATDES                   40
018900060225     D  dA1_QTAORD                   15
019000060225     D  dA1_QTADIF                   15
019100060225     D  dA1_UNIT                      3
019200060228     D*
019300060228     D wDS_A1          DS                  INZ
019400060228     D  wdA1_NUMORD                  10
019500060228     D  wdA1_POS                      6
019600060228     D  wdA1_MATNUM                  18
019700060228     D  wdA1_MATDES                  40
019800060228     D  wdA1_QTAORD                  15
019900060228     D  wdA1_QTADIF                  15
020000060228     D  wdA1_UNIT                     3
020100060225     D*
020200060225     D DS_A2           DS                  INZ
020300060225     D  dA2_CONTAT                   40
020400060225     D  dA2_TEL                      30
020500060225     D  dA2_FAX                      30
020600060225     D*
020700060225     D DS_A3           DS                  INZ
020800060225     D  dA3_MITTID                   17
020900060225     D  dA3_MITTRSD                  40
021000060225     D  dA3_MITTIND                  40
021100060225     D  dA3_MITTCAP                  10
021200060225     D  dA3_MITTLOC                  40
021300060225     D  dA3_MITTNAZ                   3
021400060225     D*
021500060225     D DS_A4           DS                  INZ
021600060227     D  dA4_DESTID                   17
021700060225     D  dA4_DESTRSD                  40
021800060225     D  dA4_DESTRD2                  40
021900060225     D  dA4_DESTIND                  40
022000060225     D  dA4_DESTCAP                  10
022100060225     D  dA4_DESTLOC                  40
022200060225     D  dA4_DESTNAZ                   3
022300060225     D  dA4_DESTPRV                   3
022400060225     D*
022500060225     D DS_A5           DS                  INZ
022600060225     D  dA5_ORIGRSD                  40
022700060225     D  dA5_ORIGIND                  40
022800060225     D  dA5_ORIGCIV                   4
022900060225     D  dA5_ORIGCAP                  10
023000060225     D  dA5_ORIGLOC                  40
023100060227     D  dA5_ORIGNAZ                   3
023200060228     D  dA5_ORIGNAZD                 25
023300060225     D*
023400060225     D DS_A6           DS                  INZ
023500060228     D  dA6_DATSPE                   10
023600060225     D*
023700060225     D DS_A7           DS                  INZ
023800060227     D  dA7_VETTRSD                  40
023900060227     D  dA7_VETTIND                  40
024000060227     D  dA7_VETTCAP                  10
024100060227     D  dA7_VETTLOC                  40
024200060227     D  dA7_VETTNAZ                   3
024300060225     D*
024400060225     D DS_A8           DS                  INZ
024500060225     D  dA8_NUMDOC                   10
024600060225     D  dA8_PORTO1                    3
024700060225     D  dA8_PORTO2                   28
024800060225     D  dA8_ROUTE                     6
024900060225     D  dA8_PESOTOT                  17
025000060225     D  dA8_PESONET                  15
025100060225     D  dA8_PESOUNIT                  3
025200060225     D  dA8_VOLTOT                   15
025300060225     D  dA8_VOLUNIT                   3
025400060225     D  dA8_COLLI                     5
025500060225     D*
025600060225     D DS_B1           DS                  INZ
025700060225     D  dB1_QTADIF                   15
025800060225     D  dB1_UNIT                      3
025900060225     D  dB1_QTAORD                   15
026000060225     D*
026100060225     D DS_B2           DS                  INZ
026200060225     D  dB2_CLIORDNUM                35
026300060228     D  dB2_CLIORDDAT                10
026400060225     D*
026500060225     D DS_B3           DS                  INZ
026600060225     D  dB3_CLIORDNUM                35
026700060228     D  dB3_CLIORDDAT                10
026800060225     D*
026900060225     D DS_B4           DS                  INZ
027000060313 www D  dB6_DETPOSNUM                 6
027100060313 www D  dB6_DETBARCOD                10
027200060225     D  dB4_DATSCAD                  30
027300060225     D*
027400060225     D DS_B4_1         DS                  INZ
027500060313 www D  dB6_DETPOSNUM                 6
027600060313 www D  dB6_DETBARCOD                10
027700060225     D  dB4_1_DATSCAD                30
027800060228     D*
027900060313     D wDS_B4          DS                  INZ
028000060313 www D  dB6_DETPOSNUM                 6
028100060313 www D  dB6_DETBARCOD                10
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
029300060313     D  dB6_DETQTASPE                15
029400060313     D  dB6_DETUNIT                   3
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
048900060313     C                   IF        VATNOT_B <> *blanks
049000060313     C                   EVAL      VATTRC = VATTRC_B
049100060313     C                   EVAL      VATNOT = VATNOT_B
049200041210     C                   WRITE     FIVAT000
049300041210     C                   ENDIF
049400060313     C                   IF        FlgDKC   =  'S'
049500060313     C                   IF        VATNOT_E <> *blanks
049600060313     C                   EVAL      VATTRC = VATTRC_E
049700060313     C                   EVAL      VATNOT = VATNOT_E
049800060313     C                   WRITE     FIVAT000
049900060313     C                   ENDIF
050000060313     C                   ENDIF
050100060227     C                   EXSR      WRIVAX
050200041210     C                   ENDIF
050300010202     C*
050400010202     C                   endif
050500990910
050600990910     C* Se non ci sono record con errori ...
050700000710     C                   if        §ctrno = 0
050800990910     C* ... restituisco esito OK.
050900990921     C                   eval      wrkesito = '0'
051000990910     C                   else
051100010201     C                   if        §ctrokvb > 0
051200990921     C                   eval      wrkesito = '1'
051300000710     C                   else
051400000710     C                   eval      wrkesito = '2'
051500990910     C                   endif
051600000710     C                   endif
051700990910     C*
051800990914     C                   if        %open(tivin00r)
051900990908     C                   close     tivin00r
052000990914     C                   endif
052100021113     C                   if        %open(fivabwwr)
052200021113     C                   close     fivabwwr
052300990914     C                   endif
052400021113     C                   if        %open(fivatwwr)
052500021113     C                   close     fivatwwr
052600010201     C                   endif
052700060225     C                   if        %open(fivaxwwr)
052800060225     C                   close     fivaxwwr
052900060225     C                   endif
053000990910     C*
053100010201     C                   if        §ctrokvb > 0
053200000724     C                             and vlrpoi <> *zeros
053300010202     C                   exsr      invio
053400990920     C                   endif
053500990920     C*
053600910830     C                   ENDSR
053700000613     C***
053800990920
053900000801     C*----------------------------------------------------*
054000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
054100000801     C*----------------------------------------------------*
054200010201     C     INZVAR        BEGSR
054300000801     C*
054400040802     C                   Z-ADD     *zeros        Num5_0            5 0
054500040802     C                   MOVEL     '0'           FlgCAS            1
054600060313     C                   MOVEL     '0'           FlgDKC            1
054700060313     C                   Z-ADD     *zeros        wQtaDif          30 9
054800060313     C                   Z-ADD     *zeros        wQtaOrd          30 9
054900060313     C                   Z-ADD     *zeros        wQtaEvasa        30 3
055000060227     C*
055100060227     C* Inizializzo tutte le schiere e le ds d wrk
055200060227     C                   CLEAR                   DS_A1
055300060227     C                   CLEAR                   DS_A2
055400060227     C                   CLEAR                   DS_A3
055500060227     C                   CLEAR                   DS_A4
055600060227     C                   CLEAR                   DS_A5
055700060227     C                   CLEAR                   DS_A6
055800060228     C***                CLEAR                   DS_A7
055900060227     C                   CLEAR                   DS_A8
056000060227     C                   CLEAR                   DS_B1
056100060227     C                   CLEAR                   DS_B2
056200060227     C                   CLEAR                   DS_B3
056300060227     C                   CLEAR                   DS_B4
056400060227     C                   CLEAR                   DS_B4_1
056500060227     C                   CLEAR                   DS_B5
056600060227     C                   CLEAR                   DS_B6
056700060227     C*
056800060228     C                   CLEAR                   skVAR_PKB4
056900060228     C                   CLEAR                   skVAR_PKB4_1
057000060228     C                   CLEAR                   skVAR_PKB6
057100060227     C                   Z-ADD     *zeros        jVAR_PK
057200060228     C                   CLEAR                   skVAR_RS
057300060227     C                   Z-ADD     *zeros        jVAR_RS
057400000801     C*
057500000801     C                   ENDSR
057600000801     C*----------------------------------------------------*
057700040910     C*  IMPOSTAZIONE CAMPI COSTANTI
057800000801     C*----------------------------------------------------*
057900000801     C     DEFCAM        BEGSR
058000000801     C*
058100021113     C                   CLEAR                   FIVAB000
058200040802     C                   CLEAR                   FIVAT000
058300060225     C                   CLEAR                   FIVAX000
058400020619     C* Imposto i valori di default...
058500041210     C                   Z-ADD     0431893       VABCCM
058600041210     C                   Z-ADD     0431893       VATCCM
058700060225     C                   Z-ADD     0431893       VAXCCM
058800041210     C                   Z-ADD     043           VABLNP
058900041210     C                   Z-ADD     043           VATLNP
059000060225     C                   Z-ADD     043           VAXLNP
059100041210     C                   Z-ADD     000           VABCTR
059200060228     C                   MOVEL     '7A'          VABCTM
059300040823     C                   MOVEL     '1'           VABCBO
059400020619     C* ... e poi verifico se sono stati passati come parametri
059500020619     C                   IF        vlrppt > *blanks
059600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
059700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
059800020619     C                   EXSR      CHKNUM
059900020619     C                   IF        PiInt=*on
060000020619     C                   Z-ADD     PiVal         VABCCM
060100020619     C                   Z-ADD     PiVal         VATCCM
060200060225     C                   Z-ADD     PiVal         VAXCCM
060300020619     C                   ENDIF
060400040506     C                   ENDIF
060500040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
060600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
060700020619     C                   EXSR      CHKNUM
060800020619     C                   IF        PiInt=*on
060900020619     C                   Z-ADD     PiVal         VABLNP
061000020619     C                   Z-ADD     PiVal         VATLNP
061100060225     C                   Z-ADD     PiVal         VAXLNP
061200040506     C                   ENDIF
061300020619     C                   ENDIF
061400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
061500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
061600020619     C                   EXSR      CHKNUM
061700020619     C                   IF        PiInt=*on
061800020619     C                   Z-ADD     PiVal         VABCTR
061900040506     C                   ENDIF
062000020619     C                   ENDIF
062100041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
062200041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
062300041210     C                   ENDIF
062400060313     C                   EVAL      FlgDKC = %subst(vlrppt:16:1)
062500020619     C                   ENDIF
062600000801     C*
062700000801     C                   ENDSR
062800000801     C*----------------------------------------------------*
062900060225     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT e FIVAX)
063000000801     C*----------------------------------------------------*
063100060225     C     IMPVBVTVX     BEGSR
063200041210     C*
063300041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
063400041210     C                   Z-ADD     *zeros        errore            1 0
063500060313     C*
063600060313     C********
063700060313     C* Tipo record 'E1EDL20' (dati testata: riferimenti, peso, volume, colli, etc..)
063800060313     C********
063900060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL20'
064000060313     C*
064100060313     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del FIVAB
064200060313     C                   IF        FlgNewBol = '1'
064300060313     C                   WRITE     FIVAB000
064400060313     C                   IF        VATNOT_B <> *blanks
064500060313     C                   EVAL      VATTRC = VATTRC_B
064600060313     C                   EVAL      VATNOT = VATNOT_B
064700060313     C                   WRITE     FIVAT000
064800060313     C                   ENDIF
064900060313     C                   IF        FlgDKC   =  'S'
065000060313     C                   IF        VATNOT_E <> *blanks
065100060313     C                   EVAL      VATTRC = VATTRC_E
065200060313     C                   EVAL      VATNOT = VATNOT_E
065300060313     C                   WRITE     FIVAT000
065400060313     C                   ENDIF
065500060313     C                   ENDIF
065600060313     C                   EXSR      WRIVAX
065700060313     C                   EVAL      FlgNewBol = '0'
065800060313     C                   ENDIF
065900060313     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
066000060313     C                   EXSR      INZVAR
066100060313     C                   EXSR      DEFCAM
066200060313     C                   EVAL      FlgNewBol = '1'
066300060313     C* Valorizzazione => DS_A8
066400060313     C                   EVALR     dA8_NUMDOC   = %trim(%subst(vindta:63+1:10))
066500060313     C                   MOVE(P)   dA8_NUMDOC    wNum10_0         10 0
066600060313     C                   EVALR     dA8_NUMDOC   = %trim(%editc(wNum10_0:'4'))
066700060313     C                   EVAL      dA8_PORTO1   = %trim(%subst(vindta:115+1:3))
066800060313     C                   EVAL      dA8_PORTO2   = %trim(%subst(vindta:118+1:28))
066900060313     C                   EVAL      dA8_ROUTE    = %trim(%subst(vindta:146+1:6))
067000060313     C                   EVALR     dA8_PESOTOT  = %trim(%subst(vindta:154+1:17))
067100060313     C                   EVAL      PiStr=dA8_PESOTOT
067200060313     C                   EXSR      CHKNUM
067300060313     C                   IF        PiNum=*on
067400060313     C                   IF        PiInt=*on
067500060313     C                   Z-ADD     PiVal         wNum30_0         30 0
067600060313     C                   EVALR     dA8_PESOTOT  = %trim(%editc(wNum30_0:'2'))
067700060313     C                   ELSE
067800060313     C                   Z-ADD     PiVal         wNum30_3         30 3
067900060313     C                   EVALR     dA8_PESOTOT  = %trim(%editc(wNum30_3:'2'))
068000060313     C                   ENDIF
068100060313     C                   ENDIF
068200060313     C                   EVALR     dA8_PESONET  = %trim(%subst(vindta:171+1:15))
068300060313     C                   EVAL      PiStr=dA8_PESONET
068400060313     C                   EXSR      CHKNUM
068500060313     C                   IF        PiNum=*on
068600060313     C                   IF        PiInt=*on
068700060313     C                   Z-ADD     PiVal         wNum30_0         30 0
068800060313     C                   EVALR     dA8_PESONET  = %trim(%editc(wNum30_0:'2'))
068900060313     C                   ELSE
069000060313     C                   Z-ADD     PiVal         wNum30_3         30 3
069100060313     C                   EVALR     dA8_PESONET  = %trim(%editc(wNum30_3:'2'))
069200060313     C                   ENDIF
069300060313     C                   ENDIF
069400060313     C                   EVAL      dA8_PESOUNIT = %trim(%subst(vindta:186+1:3))
069500060313     C                   EVALR     dA8_VOLTOT   = %trim(%subst(vindta:189+1:15))
069600060313     C                   EVAL      PiStr=dA8_VOLTOT
069700060313     C                   EXSR      CHKNUM
069800060313     C                   IF        PiNum=*on
069900060313     C                   IF        PiInt=*on
070000060313     C                   Z-ADD     PiVal         wNum30_0         30 0
070100060313     C                   EVALR     dA8_VOLTOT   = %trim(%editc(wNum30_0:'2'))
070200060313     C                   ELSE
070300060313     C                   Z-ADD     PiVal         wNum30_3         30 3
070400060313     C                   EVALR     dA8_VOLTOT   = %trim(%editc(wNum30_3:'2'))
070500060313     C                   ENDIF
070600060313     C                   ENDIF
070700060313     C                   EVAL      dA8_VOLUNIT  = %trim(%subst(vindta:204+1:3))
070800060313     C                   EVALR     dA8_COLLI    = %trim(%subst(vindta:207+1:5))
070900060313     C                   MOVE(P)   dA8_COLLI     wNum5_0           5 0
071000060313     C                   EVALR     dA8_COLLI    = %trim(%editc(wNum5_0:'2'))
071100060313     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
071200060313     C                   MOVEL     datcor        VABAAS
071300060313     C                   MOVEL     datcor        VATAAS
071400060313     C                   MOVEL     datcor        VAXAAS
071500060313     C                   MOVE      datcor        VABMGS
071600060313     C                   MOVE(P)   vlrpoi        VABFGS
071700060313     C                   MOVE(P)   vlrpoi        VATFGS
071800060313     C                   MOVE(P)   vlrpoi        VAXFGS
071900060313     C* ......VABNSP/VATNSP/VAXNSP => Stacco un numeratore da AZNUM
072000060313     C                   CLEAR                   TRUL33DS
072100060313     C                   EVAL      I33OPE = *zeros
072200060313     C                   EVAL      I33CNU = 302
072300060313     C                   EVAL      I33NUM = 1
072400060313     C                   MOVEL     TRUL33DS      KPJBU
072500060313     C                   CALL      'TRUL33R'
072600060313     C                   PARM                    KPJBA
072700060313     C                   MOVEL     KPJBU         TRUL33DS
072800060313     C                   IF        O33ERR = *zeros
072900060313     C                   Z-ADD     O33NRF        VABNSP
073000060313     C                   Z-ADD     O33NRF        VATNSP
073100060313     C                   Z-ADD     O33NRF        VAXNSP
073200060313     C                   ELSE
073300060313     C                   ADD       1             errore
073400060313     C                   EVAL      vinmsg = %trimr(vinmsg)
073500060313     C                             + ' ' + 'VABNSP VATNSP VAXNSP'
073600060313     C                   ENDIF
073700060313     C* ......VABRMN
073800060313     C                   EVAL      PiStr=%trim(%subst(vindta:64:10))
073900060313     C                   EXSR      CHKNUM
074000060313     C                   IF        PiInt=*on
074100060313     C                   Z-ADD     PiVal         VABRMN
074200060313     C                   ELSE
074300060313     C                   Z-ADD     1             VABRMN
074400060313     C                   ADD       1             errore
074500060313     C                   EVAL      vinmsg = %trimr(vinmsg)
074600060313     C                             + ' ' + 'VABRMN'
074700060313     C                   ENDIF
074800060313     C* ......VABRMA
074900060313     C                   EVAL      VABRMA=%trim(%subst(vindta:64:10))
075000060313     C* ......VABPKB
075100060313     C                   EVAL      PiStr=%trim(%subst(vindta:155:17))
075200060313     C                   EXSR      CHKNUM
075300060313     C                   IF        PiNum=*on
075400060313     C                   IF        %trim(%subst(vindta:187:3)) = 'GRM'          * da grammi a kili
075500060313     C                   EVAL      PiVal=PiVal/1000
075600060313     C                   Z-ADD     PiVal         VABPKB
075700060313     C                   ELSE
075800060313     C                   Z-ADD     PiVal         VABPKB                        * altrimenti dft. kg
075900060313     C                   ENDIF
076000060313     C                   ELSE
076100060313     C                   ADD       1             errore
076200060313     C                   EVAL      vinmsg = %trimr(vinmsg)
076300060313     C                             + ' ' + 'VABPKB'
076400060313     C                   ENDIF
076500060313     C* ......VABVLB
076600060313     C                   EVAL      PiStr=%trim(%subst(vindta:190:15))
076700060313     C                   EXSR      CHKNUM
076800060313     C                   IF        PiNum=*on
076900060313     C                   IF        %trim(%subst(vindta:205:3)) = 'DMQ'          * da dec3 a m3
077000060313     C                   EVAL      PiVal=PiVal/1000
077100060313     C                   Z-ADD     PiVal         VABVLB
077200060313     C                   ELSE
077300060313     C                   Z-ADD     PiVal         VABVLB                         * altrimenti dft. m3
077400060313     C                   ENDIF
077500060313     C                   ELSE
077600060313     C                   ADD       1             errore
077700060313     C                   EVAL      vinmsg = %trimr(vinmsg)
077800060313     C                             + ' ' + 'VABVLB'
077900060313     C                   ENDIF
078000060313     C* ......VABNCL
078100060313     C                   EVAL      PiStr=%trim(%subst(vindta:208:5))
078200060313     C                   EXSR      CHKNUM
078300060313     C                   IF        PiInt=*on
078400060313     C                   Z-ADD     PiVal         VABNCL
078500060313     C                   ELSE
078600060313     C                   ADD       1             errore
078700060313     C                   EVAL      vinmsg = %trimr(vinmsg)
078800060313     C                             + ' ' + 'VABNCL'
078900060313     C                   ENDIF
079000060313     C*
079100060313     C                   ENDIF
079200060227     C*
079300060227     C********
079400060227     C* Tipo record 'Z1EDL14'
079500060227     C********
079600060227     C                   IF        %trim(%subst(vindta:1:10)) = 'Z1EDL14'
079700060227     C* Valorizzazione => DS_A1
079800060228     C                   EVAL      dA1_NUMORD   = %trim(%subst(vindta:63+1:10))
079900060228     C                   MOVE(P)   dA1_NUMORD    wNum10_0         10 0
080000060228     C                   EVALR     dA1_NUMORD   = %trim(%editc(wNum10_0:'4'))
080100060227     C                   EVAL      dA1_POS      = %trim(%subst(vindta:73+1:6))
080200060228     C                   EVALR     dA1_MATNUM   = %trim(%subst(vindta:79+1:18))
080300060227     C                   MOVE(P)   dA1_MATNUM    wNum18_0         18 0
080400060228     C                   EVALR     dA1_MATNUM   = %trim(%editc(wNum18_0:'4'))
080500060227     C                   EVAL      dA1_MATDES   = %trim(%subst(vindta:97+1:40))
080600060228     C                   EVALR     dA1_QTAORD   = %trim(%subst(vindta:137+1:15))
080700060227     C                   EVAL      PiStr=dA1_QTAORD
080800060227     C                   EXSR      CHKNUM
080900060228     C                   IF        PiNum=*on
081000060228     C                   IF        PiInt=*on
081100060228     C                   Z-ADD     PiVal         wNum30_0         30 0
081200060228     C                   EVALR     dA1_QTAORD   = %trim(%editc(wNum30_0:'2'))
081300060228     C                   ELSE
081400060228     C                   Z-ADD     PiVal         wNum30_3         30 3
081500060228     C                   EVALR     dA1_QTAORD   = %trim(%editc(wNum30_3:'2'))
081600060228     C                   ENDIF
081700060228     C                   ENDIF
081800060228     C                   EVALR     dA1_QTADIF   = %trim(%subst(vindta:152+1:15))
081900060227     C                   EVAL      PiStr=dA1_QTADIF
082000060227     C                   EXSR      CHKNUM
082100060228     C                   IF        PiNum=*on
082200060228     C                   IF        PiInt=*on
082300060228     C                   Z-ADD     PiVal         wNum30_0         30 0
082400060228     C                   EVALR     dA1_QTADIF   = %trim(%editc(wNum30_0:'2'))
082500060228     C                   ELSE
082600060228     C                   Z-ADD     PiVal         wNum30_3         30 3
082700060228     C                   EVALR     dA1_QTADIF   = %trim(%editc(wNum30_3:'2'))
082800060228     C                   ENDIF
082900060228     C                   ENDIF
083000060227     C                   EVAL      dA1_UNIT     = %trim(%subst(vindta:167+1:3))
083100060227     C*
083200060227     C* Trattandosi d record "ripetuto" memorizzo in specifica schiera
083300060228     C                   ADD       1             jVAR_RS
083400060228     C                   EVAL      skVAR_RS(jVAR_RS) = DS_A1
083500060227     C                   ENDIF
083600060227     C*
083700060227     C********
083800060227     C* Tipo record 'Z1EDL20'
083900060227     C********
084000060227     C                   IF        %trim(%subst(vindta:1:10)) = 'Z1EDL20'
084100060227     C* Valorizzazione => DS_A2
084200060227     C                   EVAL      dA2_CONTAT   = %trim(%subst(vindta:130+1:40))
084300060227     C                   EVAL      dA2_TEL      = %trim(%subst(vindta:170+1:30))
084400060313     C* Tolgo il prefisso internazionale x l'Italia
084500060313     C                   IF        %subst(%trim(dA2_TEL):1:5) = '0039/'
084600060313     C                   EVAL      dA2_TEL      = %subst(%trim(dA2_TEL):6)
084700060313     C                   ENDIF
084800060313     C                   EVAL      dA2_FAX      = %trim(%subst(vindta:200+1:30))
084900060313     C* Tolgo il prefisso internazionale x l'Italia
085000060313     C                   IF        %subst(%trim(dA2_FAX):1:5) = '0039/'
085100060313     C                   EVAL      dA2_FAX      = %subst(%trim(dA2_FAX):6)
085200060313     C                   ENDIF
085300060227     C                   ENDIF
085400060227     C*
085500060227     C********
085600060313     C* Tipo record 'E1ADRM1'
085700060227     C* - soto tipo 'AG' => Mittente
085800060227     C********
085900060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM1' AND
086000060227     C                             %trim(%subst(vindta:64:3)) = 'AG'
086100060227     C* Valorizzazione => DS_A3
086200060228     C                   EVALR     dA3_MITTID   = %trim(%subst(vindta:67+1:17))
086300060227     C                   MOVE(P)   dA3_MITTID    wNum17_0         17 0
086400060228     C                   EVALR     dA3_MITTID   = %trim(%editc(wNum17_0:'4'))
086500060227     C                   EVAL      dA3_MITTRSD  = %trim(%subst(vindta:118+1:40))
086600060227     C                   EVAL      dA3_MITTIND  = %trim(%subst(vindta:438+1:40))
086700060227     C                   EVAL      dA3_MITTCAP  = %trim(%subst(vindta:572+1:10))
086800060227     C                   EVAL      dA3_MITTLOC  = %trim(%subst(vindta:607+1:40))
086900060227     C                   EVAL      dA3_MITTNAZ  = %trim(%subst(vindta:937+1:3))
087000060227     C                   ENDIF
087100060313     C*
087200060313     C********
087300060313     C* Tipo record 'E1ADRM1' (dati instradamento: rag. soc. dest, indirizzo dest, etc...)
087400060313     C* - soto tipo 'WE' => Destinatario Merce
087500060313     C********
087600060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM1' AND
087700060313     C                             %trim(%subst(vindta:64:3)) = 'WE'
087800060313     C* Valorizzazione => DS_A4
087900060313     C                   EVALR     dA4_DESTID   = %trim(%subst(vindta:67+1:17))
088000060313     C                   MOVE(P)   dA4_DESTID    wNum17_0         17 0
088100060313     C                   EVALR     dA4_DESTID   = %trim(%editc(wNum17_0:'4'))
088200060313     C                   EVAL      dA4_DESTRSD  = %trim(%subst(vindta:118+1:17))
088300060313     C                   EVAL      dA4_DESTRD2  = %trim(%subst(vindta:158+1:40))
088400060313     C                   EVAL      dA4_DESTIND  = %trim(%subst(vindta:438+1:40))
088500060313     C                   EVAL      dA4_DESTCAP  = %trim(%subst(vindta:572+1:10))
088600060313     C                   EVAL      dA4_DESTLOC  = %trim(%subst(vindta:607+1:40))
088700060313     C                   EVAL      dA4_DESTNAZ  = %trim(%subst(vindta:937+1:3))
088800060313     C                   EVAL      dA4_DESTPRV  = %trim(%subst(vindta:943+1:3))
088900060313     C*
089000060313     C* ......VABRSD/VABRD2
089100060313     C                   MOVEL     *blanks       wRSD             70
089200060313     C                   EVAL      wRSD = %trim(%subst(vindta:119:40)) + ' ' +
089300060313     C                                    %trim(%subst(vindta:159:40)) + ' ' +
089400060313     C                                    %trim(%subst(vindta:199:40)) + ' ' +
089500060313     C                                    %trim(%subst(vindta:239:40))
089600060313     C                   EVAL      VABRSD=%subst(wRSD:1:35)
089700060313     C                   EVAL      VABRD2=%subst(wRSD:1+35:35)
089800060313     C* ......VABIND
089900060313     C                   EVAL      VABIND=%trim(%subst(vindta:439:40)) + ' ' +
090000060313     C                                    %trim(%subst(vindta:479:40)) + ' ' +
090100060313     C                                    %trim(%subst(vindta:519:40)) + ' ' +
090200060313     C                                    %trim(%subst(vindta:559:40))
090300060313     C* ......VABCAD
090400060313     C                   EVAL      PiStr=%trim(%subst(vindta:573:10))
090500060313     C                   EXSR      CHKNUM
090600060313     C                   IF        PiInt=*on
090700060313     C                   Z-ADD     PiVal         Num5_0
090800060313     C                   MOVEL(P)  Num5_0        VABCAD
090900060313     C                   ELSE
091000060313     C                   ADD       1             errore
091100060313     C                   EVAL      vinmsg = %trimr(vinmsg)
091200060313     C                             + ' ' + 'VABCAD'
091300060313     C                   ENDIF
091400060313     C* ......VABLOD
091500060313     C                   EVAL      VABLOD=%trim(%subst(vindta:608:40)) + ' ' +
091600060313     C                                    %trim(%subst(vindta:648:40))
091700060313     C* ......VATNOT + Tipo record "B"
091800060313     C                   EVAL      VATNOT_B=%trim(%subst(vindta:748:30)) + ' ' +
091900060313     C                                      %trim(%subst(vindta:778:40))
092000060313     C                   EVAL      VATTRC_B = 'B'
092100060313     C* ......VABNZD (conversone da formato *ISO a formato "BARTOLINI"
092200060313     C                   EVAL      VABNZD=%trim(%subst(vindta:938:3))
092300060313     C                   Z-ADD     1             jNAZ
092400060313     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
092500060313     C                   IF        %found
092600060313     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
092700060313     C                   ENDIF
092800060313     C* ......VABPRD
092900060313     C                   EVAL      VABPRD=%trim(%subst(vindta:944:3))
093000060313     C*
093100060313     C                   ENDIF
093200060227     C*
093300060227     C********
093400060227     C* Tipo record 'E1ADRM1'
093500060227     C* - soto tipo 'OSO' => Mittente originale
093600060227     C********
093700060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM1' AND
093800060227     C                             %trim(%subst(vindta:64:3)) = 'OSO'
093900060227     C* Valorizzazione => DS_A5
094000060227     C                   EVAL      dA5_ORIGRSD  = %trim(%subst(vindta:118+1:40))
094100060227     C                   EVAL      dA5_ORIGIND  = %trim(%subst(vindta:438+1:40))
094200060227     C                   EVAL      dA5_ORIGCIV  = %trim(%subst(vindta:558+1:4))
094300060227     C                   EVAL      dA5_ORIGCAP  = %trim(%subst(vindta:572+1:10))
094400060227     C                   EVAL      dA5_ORIGLOC  = %trim(%subst(vindta:607+1:40))
094500060227     C                   EVAL      dA5_ORIGNAZ  = %trim(%subst(vindta:937+1:3))
094600060227     C                   Z-ADD     1             jNAZ
094700060227     C     dA5_ORIGNAZ   LOOKUP    skNAZISO(jNAZ)                         55
094800060227     C                   IF        %found
094900060227     C                   EVAL      dA5_ORIGNAZD = skNAZBARDES(jNAZ)
095000060227     C                   ENDIF
095100060227     C                   ENDIF
095200060227     C*
095300060227     C********
095400060227     C* Tipo record 'E1EDT13'
095500060227     C* - soto tipo '007' => IDoc Delivery
095600060227     C********
095700060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDT13' AND
095800060227     C                             %trim(%subst(vindta:64:3)) = '007'
095900060227     C* Valorizzazione => DS_A6
096000060227     C                   EVAL      dA6_DATSPE   = %trim(%subst(vindta:90+1:8))
096100060227     C                   EVAL      dA6_DATSPE   = %subst(dA6_DATSPE:7:2)+ '.'+
096200060227     C                                            %subst(dA6_DATSPE:5:2)+ '.'+
096300060227     C                                            %subst(dA6_DATSPE:1:4)
096400060313     C                   ENDIF
096500060313     C*
096600060313     C********
096700060313     C* Tipo record 'E1EDL24' Informazioni elementi testata spedizione
096800060313     C********
096900060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL24'
097000060313     C* Valorizzazione => DS_B6
097100060313     C                   IF        %trim(%subst(vindta:336+1:6)) <> *blanks
097200060313     C                   EVAL      dB6_DETPOSNUM= %trim(%subst(vindta:336+1:6))
097300060313     C                   ELSE
097400060313     C                   EVAL      dB6_DETPOSNUM= %trim(%subst(vindta:63+1:6))
097500060313     C                   ENDIF
097600060313     C                   EVALR     dB6_DETMATNUM= %trim(%subst(vindta:69+1:18))
097700060313     C                   MOVE(P)   dB6_DETMATNUM wNum18_0         18 0
097800060313     C                   EVALR     dB6_DETMATNUM= %trim(%editc(wNum18_0:'4'))
097900060313     C                   EVAL      dB6_DETMATDES= %trim(%subst(vindta:105+1:40))
098000060313     C                   EVAL      dB6_DETBARCOD= %trim(%subst(vindta:207+1:10))
098100060313     C                   EVALR     dB6_DETQTASPE= %trim(%subst(vindta:239+1:15))
098200060313     C                   EVAL      PiStr=dB6_DETQTASPE
098300060313     C                   EXSR      CHKNUM
098400060313     C                   IF        PiNum=*on
098500060313     C                   IF        PiInt=*on
098600060313     C                   Z-ADD     PiVal         wNum30_0         30 0
098700060313     C                   EVALR     dB6_DETQTASPE= %trim(%editc(wNum30_0:'2'))
098800060313     C                   ELSE
098900060313     C                   Z-ADD     PiVal         wNum30_3         30 3
099000060313     C                   EVALR     dB6_DETQTASPE= %trim(%editc(wNum30_3:'2'))
099100060313     C                   ENDIF
099200060313     C                   ENDIF
099300060313     C                   EVAL      dB6_DETUNIT  = %trim(%subst(vindta:254+1:3))
099400060313     C*
099500060313     C* Trattandosi d record "ripetuto" memorizzo in specifica schiera
099600060313     C                   ADD       1             jVAR_PK
099700060313     C                   EVAL      skVAR_PKB6(jVAR_PK) = DS_B6
099800060313     C                   ENDIF
099900060313     C*
100000060313     C********
100100060313     C* Tipo record 'Z1EDL24'
100200060313     C********
100300060313     C                   IF        %trim(%subst(vindta:1:10)) = 'Z1EDL24'
100400060313     C* Valorizzazione => DS_B1
100500060313     C                   EVALR     dB1_QTADIF   = %trim(%subst(vindta:164+1:15))
100600060313     C                   EVAL      PiStr=dB1_QTADIF
100700060313     C                   EXSR      CHKNUM
100800060313     C                   IF        PiNum=*on
100900060313     C                   IF        PiInt=*on
101000060313     C                   Z-ADD     PiVal         wNum30_0         30 0
101100060313     C                   EVALR     dB1_QTADIF   = %trim(%editc(wNum30_0:'2'))
101200060313     C                   ELSE
101300060313     C                   Z-ADD     PiVal         wNum30_3         30 3
101400060313     C                   EVALR     dB1_QTADIF   = %trim(%editc(wNum30_3:'2'))
101500060313     C                   ENDIF
101600060313     C                   ENDIF
101700060313     C                   Z-ADD     PiVal         wQtaDif
101800060313     C                   EVAL      dB1_UNIT     = %trim(%subst(vindta:179+1:3))
101900060313     C                   EVALR     dB1_QTAORD   = %trim(%subst(vindta:208+1:15))
102000060313     C                   EVAL      PiStr=dB1_QTAORD
102100060313     C                   EXSR      CHKNUM
102200060313     C                   IF        PiNum=*on
102300060313     C                   IF        PiInt=*on
102400060313     C                   Z-ADD     PiVal         wNum30_0         30 0
102500060313     C                   EVALR     dB1_QTAORD   = %trim(%editc(wNum30_0:'2'))
102600060313     C                   ELSE
102700060313     C                   Z-ADD     PiVal         wNum30_3         30 3
102800060313     C                   EVALR     dB1_QTAORD   = %trim(%editc(wNum30_3:'2'))
102900060313     C                   ENDIF
103000060313     C                   Z-ADD     PiVal         wQtaOrd
103100060313     C                   ENDIF
103200060313     C                   ENDIF
103300060313     C*
103400060313     C********
103500060313     C* Tipo record 'E1EDL15'
103600060313     C* - soto tipo '9999999400' => IDoc Caratteristiche
103700060313     C********
103800060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL15' AND
103900060313     C                             %trim(%subst(vindta:64:10)) = '9999999400'
104000060313     C* Valorizzazione => DS_B4
104100060313     C                   EVAL      dB4_DATSCAD  = %trim(%subst(vindta:133+1:30))
104200060313     C*
104300060313     C* Trattandosi d record "ripetuto" memorizzo in specifica schiera
104400060313     C                   EVAL      skVAR_PKB4(jVAR_PK) = DS_B4
104500060313     C                   ENDIF
104600060313     C*
104700060313     C********
104800060313     C* Tipo record 'E1EDL15'
104900060313     C* - soto tipo '0000000823' => IDoc Caratteristiche
105000060313     C********
105100060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL15' AND
105200060313     C                             %trim(%subst(vindta:64:10)) = '0000000823'
105300060313     C* Valorizzazione => DS_B4_1
105400060313     C                   EVAL      dB4_1_DATSCAD= %trim(%subst(vindta:133+1:30))
105500060313     C*
105600060313     C* Trattandosi d record "ripetuto" memorizzo in specifica schiera
105700060313     C                   EVAL      skVAR_PKB4_1(jVAR_PK) = DS_B4_1
105800060313     C                   ENDIF
105900060313     C*
106000060313     C********
106100060313     C* Tipo record 'E1EDL43'
106200060313     C* - soto tipo 'C' => IDoc Riferimenti
106300060313     C********
106400060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL43' AND
106500060313     C                             %trim(%subst(vindta:64:1)) = 'C'
106600060313     C* Valorizzazione => DS_B5
106700060313     C***                EVAL      dB5_IDOCNUM  = %trim(%subst(vindta:64+1:35))
106800060313     C                   EVAL      dB5_IDOCNUM  = %trim(%subst(vindta:64+1:10))
106900060313     C                   MOVE(P)   dB5_IDOCNUM   wNum10_0         10 0
107000060313     C                   EVALR     dB5_IDOCNUM  = %trim(%editc(wNum10_0:'4'))
107100060313     C                   ENDIF
107200060313     C*
107300060313     C********
107400060313     C* Tipo record 'E1EDL41'
107500060313     C* - soto tipo '001' => Riferimenti e date ordini - cliente
107600060313     C********
107700060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL41' AND
107800060313     C                             %trim(%subst(vindta:64:3)) = '001'
107900060313     C* Valorizzazione => DS_B2
108000060313     C                   EVAL      dB2_CLIORDNUM= %trim(%subst(vindta:66+1:35))
108100060313     C                   EVAL      dB2_CLIORDDAT= %trim(%subst(vindta:101+1:8))
108200060313     C                   EVAL      dB2_CLIORDDAT= %subst(dB2_CLIORDDAT:7:2)+'.'+
108300060313     C                                            %subst(dB2_CLIORDDAT:5:2)+'.'+
108400060313     C                                            %subst(dB2_CLIORDDAT:1:4)
108500060313     C                   ENDIF
108600060313     C*
108700060313     C********
108800060313     C* Tipo record 'E1EDL41'
108900060313     C* - soto tipo '002' => Riferimenti e date ordini - evasione mittente
109000060313     C********
109100060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL41' AND
109200060313     C                             %trim(%subst(vindta:64:3)) = '002'
109300060313     C* Valorizzazione => DS_B3
109400060313     C                   EVAL      dB3_CLIORDNUM= %trim(%subst(vindta:66+1:35))
109500060313     C                   EVAL      dB3_CLIORDDAT= %trim(%subst(vindta:101+1:8))
109600060313     C                   EVAL      dB3_CLIORDDAT= %subst(dB3_CLIORDDAT:7:2)+'.'+
109700060313     C                                            %subst(dB3_CLIORDDAT:5:2)+'.'+
109800060313     C                                            %subst(dB3_CLIORDDAT:1:4)
109900060313     C                   ENDIF
110000060227     C*
110100060227     C********
110200060313     C* Tipo record 'E1ADRM4'
110300060227     C* - soto tipo 'SP' => Vettore
110400060227     C********
110500060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM4' AND
110600060227     C                             %trim(%subst(vindta:64:3)) = 'SP'
110700060227     C* Valorizzazione => DS_A7
110800060227     C                   EVAL      dA7_VETTRSD  = %trim(%subst(vindta:118+1:40))
110900060227     C                   EVAL      dA7_VETTIND  = %trim(%subst(vindta:438+1:40))
111000060227     C                   EVAL      dA7_VETTCAP  = %trim(%subst(vindta:572+1:10))
111100060227     C                   EVAL      dA7_VETTLOC  = %trim(%subst(vindta:607+1:40))
111200060227     C                   EVAL      dA7_VETTNAZ  = %trim(%subst(vindta:937+1:3))
111300060227     C                   ENDIF
111400060313     C*
111500060313     C********
111600060313     C* Tipo record 'E1EDL37' Informazioni "CHI SONO" colli
111700060313     C********
111800060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL37'
111900060313     C                   EVAL      VATNOT_E = %trim(%subst(vindta:1+1:20))
112000060313     C                   EVAL      VATTRC_E = 'E'
112100060313     C                   ENDIF
112200040910     C*
112300040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
112400040910     C                   IF        FlgCAS <> '0'
112500040929     C                   IF        VABCBO = '1'
112600040910     C                   EVAL      VABCBO = '4'
112700040910     C                   ELSE
112800040929     C                   EVAL      VABCBO = '6'
112900040910     C                   ENDIF
113000040929     C                   ENDIF
113100040910     C*
113200040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
113300040910     C                   EXSR      CHKIMPDIV
113400010202     C*
113500000801     C* Ebbene...
113600000801     C                   ADD       1             §CTRMO
113700010201     C                   IF        errore <> *zeros
113800000801     C                   ADD       1             §CTRNO
113900000801     C                   EVAL      recko = vindta
114000000801     C                   EXCEPT    rigadett
114100000801     C                   EVAL      vinflg = '2'
114200000801     C                   ELSE
114300010201     C                   ADD       1             §CTROKVB
114400000801     C                   ENDIF
114500000801     C*
114600000801     C                   ENDSR
114700010202     C*----------------------------------------------------*
114800060225     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR E FILE FIVAXWWR
114900010202     C*----------------------------------------------------*
115000060225     C     PREVATVAX     BEGSR
115100010202     C*
115200060225     C* Compongo il nome del membro da dare al FIVATWWR e al FIVAXWWR
115300010202     C                   eval      parmbr = vlrhdl
115400010202     C                   movel     'M'           parmbr
115500041210     C                   eval      parccm = vlrksc
115600010202     C                   eval      paropz = '1'
115700010202     C* Effettuo la chiamata al CLLE preposto
115800060225     C                   call(e)   'TITVVTXC'
115900010202     C                   parm                    parccm
116000010202     C                   parm                    parmbr
116100010202     C                   parm                    paropz
116200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
116300010202     C                   if        %error
116400010202     C                   movel     '1'           chkcall
116500010202     C                   else
116600010202     C                   movel     '0'           chkcall
116700010202     C                   endif
116800010202     C*
116900010202     C                   ENDSR
117000000801     C*----------------------------------------------------*
117100000801     C*  CONTROLLO NUMERICITA' CAMPI
117200000801     C*----------------------------------------------------*
117300000801     C     CHKNUM        BEGSR
117400000801     C*
117500000801     C                   call(e)   'ISNUMERIC'
117600000801     C                   PARM                    PiStr            30
117700041210     C                   PARM      '.'           PiDecChr          1
117800000801     C                   PARM      *ZEROS        PiVal            30 9
117900000801     C                   PARM      '0'           PiInt             1
118000000801     C                   PARM      '0'           PiNum             1
118100000801     C                   IF        %error
118200060227     C                   EVAL      PiNum=*off
118300000801     C                   ENDIF
118400000801     C*
118500000801     C                   ENDSR
118600000801     C***
118700000801
118800011113
118900011113     C*----------------------------------------------------*
119000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
119100011113     C*----------------------------------------------------*
119200011113     C     CHKIMPDIV     BEGSR
119300011113     C*
119400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
119500011113     C                   Z-ADD     *zeros        wrkDec            9 9
119600011113     C*
119700011113     C* Come prima cosa effettuo considerazioni sulla divisa
119800011113     C                   IF        vabIAS > *zeros
119900011113     C                   IF        vabVAS <> 'EUR'
120000011113     C                   EVAL      vabVAS =  'ITL'
120100011113     C                   ENDIF
120200011113     C                   ENDIF
120300011113     C*
120400011113     C                   IF        vabCAS > *zeros
120500011113     C                   IF        vabVCA <> 'EUR'
120600011113     C                   EVAL      vabVCA =  'ITL'
120700011113     C                   ENDIF
120800011113     C                   ENDIF
120900011113     C*
121000011113     C                   IF        vabVMD > *zeros
121100020305     C                   IF        vabVAD <> 'EUR'
121200011113     C                   EVAL      vabVAD =  'ITL'
121300011113     C                   ENDIF
121400011113     C                   ENDIF
121500011113     C*
121600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
121700011113     C                   Z-ADD     vabIAS        wrkDec
121800011113     C                   IF        wrkDec > *zeros
121900011113     C                   IF        vabVAS = 'ITL'
122000011113     C                   EVAL      vabIAS = *zeros
122100011113     C                   ENDIF
122200011113     C                   ENDIF
122300011113     C*
122400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
122500011113     C                   Z-ADD     vabCAS        wrkDec
122600011113     C                   IF        wrkDec > *zeros
122700011113     C                   IF        vabVCA = 'ITL'
122800011113     C                   EVAL      vabCAS = *zeros
122900011113     C                   ENDIF
123000011113     C                   ENDIF
123100011113     C*
123200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
123300011113     C                   Z-ADD     vabVMD        wrkDec
123400011113     C                   IF        wrkDec > *zeros
123500011113     C                   IF        vabVAD = 'ITL'
123600011113     C                   EVAL      vabVMD = *zeros
123700011113     C                   ENDIF
123800011113     C                   ENDIF
123900011113     C*
124000011113     C                   ENDSR
124100011113     C***
124200011113
124300060227
124400060227
124500060227     C*----------------------------------------------------*
124600060227     C*  ROUTINE GENERAZIONE BUFFER FORMATI FILE FIVAXWWR
124700060227     C*----------------------------------------------------*
124800060227     C     WRIVAX        BEGSR
124900060313     C*
125000060313     C* Inizializzo deposito per verifica cambio posizioni articoli/ordini
125100060313     C                   MOVEL     *blanks       wCurrPosiz        6
125200060227     C*
125300060227     C* Inizializzo contatore d riga e flag fine dati DDT x bolla corrente
125400060227     C                   Z-ADD     1             VAXPRR
125500060227     C*
125600060227     C* Imposto il tipo modulo x la stampa del logo personalizzato
125700060227     C                   EVAL      VAXTMD = 'BBRA'
125800060227     C*
125900060227     C* Innanzitutto inizializzo il campo dati flat
126000060227     C                   CLEAR                   VAXDTT
126100060228     C*
126200060228     C* Imposto il carattere in "grassetto"
126300060228     C                   EVAL      VAXTRK = 'T1'
126400060227     C*
126500060227     C                   RESET                   DS__T_1
126600060313     C                   EVAL      sT1_MITT = %trim(dA3_MITTRSD)
126700060227     C                   EVAL      sT1_DEST = %trim(dA4_DESTRSD)
126800060227     C                   EVAL      VAXDTT   = DS__T_1
126900060228     C                   WRITE     FIVAX000
127000060228     C                   ADD       1             VAXPRR
127100060227     C*
127200060227     C                   RESET                   DS__T_1
127300060313     C                   EVAL      sT1_MITT = %trim(dA3_MITTIND)
127400060227     C                   EVAL      sT1_DEST = %trim(dA4_DESTRD2)
127500060227     C                   EVAL      VAXDTT   = DS__T_1
127600060228     C                   WRITE     FIVAX000
127700060228     C                   ADD       1             VAXPRR
127800060227     C*
127900060227     C                   RESET                   DS__T_1
128000060313     C                   EVAL      sT1_MITT = %trim(dA3_MITTCAP) + ' ' +
128100060313     C                                        %trim(dA3_MITTLOC)
128200060227     C                   EVAL      sT1_DEST = %trim(dA4_DESTIND)
128300060227     C                   EVAL      VAXDTT   = DS__T_1
128400060228     C                   WRITE     FIVAX000
128500060228     C                   ADD       1             VAXPRR
128600060227     C*
128700060227     C                   RESET                   DS__T_1
128800060313     C                   EVAL      sT1_DEST = %trim(dA4_DESTCAP) + ' '  +
128900060227     C                                        %trim(dA4_DESTLOC) + ' ' +
129000060313     C                                        '(' + %trim(dA4_DESTPRV) + ') - '+
129100060313     C                                        %trim(dA4_DESTNAZ)
129200060227     C                   EVAL      VAXDTT   = DS__T_1
129300060228     C                   WRITE     FIVAX000
129400060228     C                   ADD       1             VAXPRR
129500060227     C*
129600060228     C* Imposto il carattere in "normale"
129700060228     C                   EVAL      VAXTRK = 'C1'
129800060228     C*
129900060227     C                   EVAL      VAXDTT   = *blanks
130000060228     C                   WRITE     FIVAX000
130100060228     C                   ADD       1             VAXPRR
130200060227     C*
130300060227     C                   RESET                   DS__T_2
130400060227     C                   EVAL      sT2_TES1_1 = 'Nr.Documento:'
130500060227     C                   EVAL      sT2_TES1_2 = %trim(dA8_NUMDOC)
130600060227     C                   EVAL      sT2_TES2_1 = 'Nr.Ordine vendita:'
130700060313     C***                EVAL      sT2_TES2_2 = %trim(dA1_NUMORD)
130800060313     C                   EVAL      sT2_TES2_2 = %trim(dB5_IDOCNUM)
130900060227     C                   EVAL      VAXDTT   = DS__T_2
131000060228     C                   WRITE     FIVAX000
131100060228     C                   ADD       1             VAXPRR
131200060227     C*
131300060227     C                   RESET                   DS__T_2
131400060227     C                   EVAL      sT2_TES1_1 = 'Data spedizione:'
131500060227     C                   EVAL      sT2_TES1_2 = %trim(dA6_DATSPE)
131600060227     C                   EVAL      sT2_TES2_1 = 'Codice BBMI:'
131700060227     C                   EVAL      sT2_TES2_2 = %trim(dA4_DESTID)
131800060227     C                   EVAL      VAXDTT   = DS__T_2
131900060228     C                   WRITE     FIVAX000
132000060228     C                   ADD       1             VAXPRR
132100060227     C*
132200060227     C                   RESET                   DS__T_2
132300060227     C                   EVAL      sT2_TES1_1 = 'Itinerario:'
132400060227     C                   EVAL      sT2_TES1_2 = %trim(dA8_ROUTE)
132500060227     C                   EVAL      sT2_TES2_1 = 'Committente:'
132600060313     C                   EVAL      sT2_TES2_2 = %trim(dA3_MITTID)
132700060227     C                   EVAL      VAXDTT   = DS__T_2
132800060228     C                   WRITE     FIVAX000
132900060228     C                   ADD       1             VAXPRR
133000060227     C*
133100060227     C                   RESET                   DS__T_2
133200060313     C                   EVAL      sT2_TES1_1 = 'Tipo porto:'
133300060313     C                   EVAL      sT2_TES1_2 = %trim(dA8_PORTO1) + ' '+
133400060313     C                                          %trim(dA8_PORTO2)
133500060313     C                   EVAL      sT2_TES2_1 = 'Nr.Ordine commit.:'
133600060313     C                   EVAL      sT2_TES2_2 = %trim(dB2_CLIORDNUM)+' del: '+
133700060313     C                                          %trim(dB2_CLIORDDAT)
133800060227     C                   EVAL      VAXDTT   = DS__T_2
133900060228     C                   WRITE     FIVAX000
134000060228     C                   ADD       1             VAXPRR
134100060227     C*
134200060227     C                   RESET                   DS__T_2
134300060227     C                   EVAL      sT2_TES2_1 = 'Referente:'
134400060313     C                   EVAL      sT2_TES2_2 = %trim(dA2_CONTAT)+' Tel '+
134500060313     C                                          %trim(dA2_TEL)   +' Fax '+
134600060313     C                                          %trim(dA2_FAX)
134700060227     C                   EVAL      VAXDTT   = DS__T_2
134800060228     C                   WRITE     FIVAX000
134900060228     C                   ADD       1             VAXPRR
135000060227     C*
135100060227     C                   EVAL      VAXDTT   = *blanks
135200060228     C                   WRITE     FIVAX000
135300060228     C                   ADD       1             VAXPRR
135400060313     C*
135500060313     C                   RESET                   DS__D_3
135600060313     C***                EVAL      sD3_DESCR  = %trim(sD3_DESCR)   + ' '    +
135700060313     C***                                       %trim(dA7_VETTRSD) + ' - '  +
135800060313     C***                                       %trim(dA7_VETTIND) + ' - '  +
135900060313     C***                                       %trim(dA7_VETTCAP) + ' '    +
136000060313     C***                                       %trim(dA7_VETTLOC)
136100060313     C                   EVAL      sD3_DESCR  = %trim(sD3_DESCR)   + ' '       +
136200110504     C                                          'BRT S.p.A. - Via Monte '      +
136300060313     C                                          'Pastello, 30 - 37057 San '    +
136400060313     C                                          'Giovanni Lupatoto (VT) - IT '
136500060313     C                   EVAL      VAXDTT   = DS__D_3
136600060313     C                   WRITE     FIVAX000
136700060313     C                   ADD       1             VAXPRR
136800060313     C*
136900060313     C                   EVAL      VAXDTT   = *blanks
137000060313     C                   WRITE     FIVAX000
137100060313     C                   ADD       1             VAXPRR
137200060313     C*
137300060313     C* Imposto il carattere in "normale + grassetto"
137400060313     C                   EVAL      VAXTRK = 'T2'
137500060313     C*
137600060227     C                   RESET                   DS__D_1
137700060227     C                   EVAL      VAXDTT   = DS__D_1
137800060228     C                   WRITE     FIVAX000
137900060228     C                   ADD       1             VAXPRR
138000060313     C*
138100060313     C* Imposto il carattere in "normale"
138200060313     C                   EVAL      VAXTRK = 'C1'
138300060228     C*
138400060228     C                   RESET                   DS__R_1
138500060228     C                   EVAL      VAXDTT   = DS__R_1
138600060228     C                   WRITE     FIVAX000
138700060228     C                   ADD       1             VAXPRR
138800060227     C*
138900060227     C                   EVAL      VAXDTT   = *blanks
139000060228     C                   WRITE     FIVAX000
139100060228     C                   ADD       1             VAXPRR
139200060228     C*
139300060228     C* Se presenti informazioni sui dettagli Packing List => gestisco....
139400060228     C                   IF        jVAR_PK > *zeros
139500060313     C*
139600060228     C* ...ciclo su dettaglio (<n> righe)
139700060228     C                   Z-ADD     *zeros        jWRK              3 0
139800060228     C                   DO        *hival
139900060228     C*
140000060228     C* Incremento l'indice della schiera corrente
140100060228     C                   ADD       1             jWRK
140200060228     C*
140300060228     C* Finchè memorizzato qualcosa...  altrimenti esco dal ciclo
140400060228     C                   IF        skVAR_PKB6(jWRK) <> *blanks
140500060313     C*
140600060313     C* Valorizzo la ds dei dettagli articoli/ordine
140700060313     C                   EVAL      wDS_B6 = skVAR_PKB6(jWRK)
140800060313     C*
140900060313     C* Stampo le seguenti 3 righe solo a cambio d posizione articoli/ordine
141000060313 xxx C***                IF        %trim(wCurrPosiz) <> %trim(wdB6_DETPOSNUM)
141100060313     C*
141200060313     C* Salvo la posizione articolo/ordine corrente
141300060313     C                   EVAL      wCurrPosiz = %trim(wdB6_DETPOSNUM)
141400060313     C*
141500060313     C* Imposto il carattere in "normale + grassetto"
141600060313     C                   EVAL      VAXTRK = 'T2'
141700060313     C                   RESET                   DS__D_6
141800060313     C                   EVAL      sD6_POSNUM = %trim(wdB6_DETPOSNUM)
141900060313     C                   EVAL      sD6_MATNUM = %trim(wdB6_DETMATNUM)
142000060313 www C                   EVAL      sD6_QTAORD = %trim(dB1_QTAORD) + ' ' +
142100060313 www C                                          %trim(dB1_UNIT)
142200060313     C                   IF        dB1_QTADIF = *blanks
142300060313     C                   EVAL      dB1_QTADIF = '0'
142400060313     C                   ENDIF
142500060313 www C                   EVAL      sD6_QTARES = %trim(dB1_QTADIF) + ' ' +
142600060313 www C                                          %trim(dB1_UNIT)
142700060313     C*
142800060313     C* Calcolo la quantità evasa correntemente come differenza tra quantità ordinata e residua
142900060313     C                   EVAL      wQtaEvasa  = wQtaOrd - wQtaDif
143000060313     C***                EVALR     sD6_QTA   = %trim(%editc(wQtaEvasa:'2'))+' '+
143100060313     C***                                      %trim(dB1_UNIT)
143200060313     C*
143300060313     C                   EVAL      VAXDTT   = DS__D_6
143400060313     C                   WRITE     FIVAX000
143500060313     C                   ADD       1             VAXPRR
143600060313     C*
143700060313     C* ...descrizione (riga 25)
143800060313     C                   RESET                   DS__D_7
143900060313     C                   EVAL      sD7_DETMATDES=%trim(wdB6_DETMATDES)
144000060313     C                   EVAL      VAXDTT   = DS__D_7
144100060313     C                   WRITE     FIVAX000
144200060313     C                   ADD       1             VAXPRR
144300060313     C*
144400060313     C* Imposto il carattere in "normale"
144500060313     C                   EVAL      VAXTRK = 'C1'
144600060313     C*
144700060313     C* ...riferimenti ordine cliente (riga 26)
144800060313     C                   RESET                   DS__D_8
144900060313     C                   EVAL      sD8_CLIORDNUM=%trim(dB3_CLIORDNUM)
145000060313     C                   EVAL      sD8_CLIORDDAT=%trim(dB3_CLIORDDAT)
145100060313     C                   EVAL      VAXDTT   = DS__D_8
145200060313     C                   WRITE     FIVAX000
145300060313     C                   ADD       1             VAXPRR
145400060313     C*
145500060313 xxx C***                ENDIF
145600060313     C*
145700060228     C                   IF        skVAR_PKB4_1(jWRK) <> *blanks
145800060228     C                   EVAL      wDS_B4 = skVAR_PKB4_1(jWRK)
145900060228     C                   ELSE
146000060228     C                   EVAL      wDS_B4 = skVAR_PKB4(jWRK)
146100060228     C                   ENDIF
146200060228     C                   EVAL      wDS_B6 = skVAR_PKB6(jWRK)
146300060228     C                   RESET                   DS__D_9
146400060228     C                   EVAL      sD9_DETBARCOD=%trim(wdB6_DETBARCOD)
146500060228     C                   EVAL      sD9_DATSCAD  =%trim(wdB4_DATSCAD)
146600060313     C                   EVAL      sD9_QTASPED  =%trim(dB6_DETQTASPE) + ' ' +
146700060313     C                                           %trim(dB6_DETUNIT)
146800060228     C                   EVAL      VAXDTT   = DS__D_9
146900060313     C*
147000060313     C* Se presenti dati => stampo
147100060313     C                   IF        %trim(wdB6_DETBARCOD) <> *blanks OR
147200060313     C                             %trim(wdB4_DATSCAD)   <> *blanks
147300060228     C*
147400060228     C* Quindi scarico il buffer della riga corrente del DDT bolla corrente
147500060228     C                   WRITE     FIVAX000
147600060228     C*
147700060228     C* Incremento il contatore d riga corrente
147800060228     C                   ADD       1             VAXPRR
147900060313     C*
148000060313     C                   ENDIF
148100060228     C*
148200060228     C                   ELSE
148300060228     C*
148400060228     C* Al primo elemento d schiera nn valorizzato => esco dal ciclo...
148500060228     C                   LEAVE
148600060228     C                   ENDIF
148700060228     C*
148800060228     C                   ENDDO
148900060228     C                   ENDIF
149000060228     C*
149100060228     C                   EVAL      VAXDTT   = *blanks
149200060228     C                   WRITE     FIVAX000
149300060228     C                   ADD       1             VAXPRR
149400060227     C*
149500060227     C* Se presenti informazioni sul "Residuo da evadere" => gestisco....
149600060227     C                   IF        jVAR_RS > *zeros
149700060228     C* ...titolo  (riga 26)
149800060227     C                   RESET                   DS__E_1
149900060227     C                   EVAL      VAXDTT   = DS__E_1
150000060228     C                   EVAL      VAXTRK = 'T1'                                * vado in grassetto
150100060227     C                   WRITE     FIVAX000
150200060228     C                   ADD       1             VAXPRR
150300060228     C                   EVAL      VAXTRK = 'C1'                                * torno a normale
150400060228     C* ...intestazioni colonne (riga 27)
150500060227     C                   RESET                   DS__E_2
150600060227     C                   EVAL      VAXDTT   = DS__E_2
150700060227     C                   WRITE     FIVAX000
150800060228     C                   ADD       1             VAXPRR
150900060228     C* ...riga separazione "alta" (riga 28)
151000060228     C                   RESET                   DS__R_1
151100060228     C                   EVAL      VAXDTT   = DS__R_1
151200060227     C                   WRITE     FIVAX000
151300060228     C                   ADD       1             VAXPRR
151400060228     C* ...ciclo su dettaglio (<n> righe)
151500060228     C                   Z-ADD     *zeros        jWRK              3 0
151600060228     C                   DO        *hival
151700060228     C*
151800060228     C* Incremento l'indice della schiera corrente
151900060228     C                   ADD       1             jWRK
152000060228     C*
152100060228     C* Finchè memorizzato qualcosa...  altrimenti esco dal ciclo
152200060228     C                   IF        skVAR_RS(jWRK) <> *blanks
152300060228     C                   EVAL      wDS_A1 = skVAR_RS(jWRK)
152400060228     C                   RESET                   DS__E_3
152500060228     C                   EVAL      sE3_NUMORD = %trim(wdA1_NUMORD)
152600060228     C                   EVAL      sE3_POS    = %trim(wdA1_POS)
152700060228     C                   EVAL      sE3_MATNUM = %trim(wdA1_MATNUM)
152800060228     C                   EVAL      sE3_MATDES = %trim(wdA1_MATDES)
152900060228     C                   EVAL      sE3_QTAORD = %trim(wdA1_QTAORD) + ' ' +
153000060228     C                                          %trim(wdA1_UNIT)
153100060228     C                   EVAL      sE3_QTADIF = %trim(wdA1_QTADIF) + ' ' +
153200060228     C                                          %trim(wdA1_UNIT)
153300060228     C                   EVAL      VAXDTT   = DS__E_3
153400060228     C*
153500060228     C* Quindi scarico il buffer della riga corrente del DDT bolla corrente
153600060228     C                   WRITE     FIVAX000
153700060228     C*
153800060228     C* Incremento il contatore d riga corrente
153900060228     C                   ADD       1             VAXPRR
154000060228     C*
154100060228     C                   ELSE
154200060228     C*
154300060228     C* Al primo elemento d schiera nn valorizzato => esco dal ciclo...
154400060228     C                   LEAVE
154500060228     C                   ENDIF
154600060227     C*
154700060227     C                   ENDDO
154800060228     C                   ENDIF
154900060313     C*
155000060228     C                   RESET                   DS__R_1
155100060228     C                   EVAL      VAXDTT   = DS__R_1
155200060228     C                   WRITE     FIVAX000
155300060228     C                   ADD       1             VAXPRR
155400060313     C*
155500060313     C* Imposto il carattere in "normale + grassetto"
155600060313     C                   EVAL      VAXTRK = 'T2'
155700060313     C*
155800060313     C                   RESET                   DS__P_1
155900060228     C                   EVAL      sP1_DICITUR1='Peso totale:'
156000060228     C                   EVAL      sP1_VALORE1 =%trim(dA8_PESOTOT) + ' ' +
156100060228     C                                          %trim(dA8_PESOUNIT)
156200060313     C                   EVAL      sP1_DICITUR2='Peso netto:'
156300060313     C                   EVAL      sP1_VALORE2 =%trim(dA8_PESONET) + ' ' +
156400060313     C                                          %trim(dA8_PESOUNIT)
156500060313     C                   EVAL      sP1_DICITUR3='Volume totale:'
156600060313     C                   EVAL      sP1_VALORE3 =%trim(dA8_VOLTOT) + ' ' +
156700060313     C                                          %trim(dA8_VOLUNIT)
156800060313     C                   EVAL      sP1_DICITUR4='Numero colli:'
156900060313     C                   EVAL      sP1_VALORE4 =%trim(dA8_COLLI)
157000060228     C                   EVAL      VAXDTT   = DS__P_1
157100060228     C                   WRITE     FIVAX000
157200060228     C                   ADD       1             VAXPRR
157300060313     C*
157400060313     C* Imposto il carattere in "normale"
157500060313     C                   EVAL      VAXTRK = 'C1'
157600060228     C*
157700060228     C                   RESET                   DS__R_1
157800060228     C                   EVAL      VAXDTT   = DS__R_1
157900060228     C                   WRITE     FIVAX000
158000060228     C                   ADD       1             VAXPRR
158100060227     C*
158200060227     C                   ENDSR
158300011113
158400000801
158500000801
158600990920      /TITLE Invio dei dati al punto operativo.
158700010202     C     invio         BEGSR
158800060225     C*
158900060225     C* 1° invio FIVAX
159000060225     C                   reset                   dscmz
159100060225     C                   move      vlrpoi        cmzdst
159200060225     C                   eval      cmzfld = 'FIVAXWWR'
159300060225     C                   eval      cmzmbd = vlrhdl
159400060225     C                   eval      %subst(cmzmbd:1:1) = 'M'
159500060225     C***                if        prmfir = *blanks
159600060225     C                   eval      cmzfla = 'FIVAX00F'
159700060225     C                   eval      cmzmba = 'FIVAX00F'
159800060225     C***                else
159900060225     C***                eval      cmzfla = prmfir
160000060225     C***                eval      cmzmba = prmfir
160100060225     C***                endif
160200060225     C                   eval      cmznrr = *zeros
160300060227     C                   move      §ctrokvx      cmznrr
160400060225     C                   eval      cmzlba = vlrfl1
160500060225     C                   call(e)   'TIS711C'
160600060225     C                   parm                    dscmz
160700060225     C                   parm      *blanks       esito
160800060225     C                   if        %error
160900060225     C                             or cmzerr = '1'
161000060225     C                             or esito  = '1'
161100060225     C                   eval      wrkesito = '3'
161200060225     C                   else
161300990920     C*
161400060225     C* 2° invio FIVAT
161500010201     C                   reset                   dscmz
161600010201     C                   move      vlrpoi        cmzdst
161700021113     C                   eval      cmzfld = 'FIVATWWR'
161800010201     C                   eval      cmzmbd = vlrhdl
161900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
162000021009     C***                if        prmfir = *blanks
162100021113     C                   eval      cmzfla = 'FIVAT00F'
162200021113     C                   eval      cmzmba = 'FIVAT00F'
162300021009     C***                else
162400021009     C***                eval      cmzfla = prmfir
162500021009     C***                eval      cmzmba = prmfir
162600021009     C***                endif
162700010201     C                   eval      cmznrr = *zeros
162800020305     C                   move      §ctrokvt      cmznrr
162900021018     C                   eval      cmzlba = vlrfl1
163000010201     C                   call(e)   'TIS711C'
163100010201     C                   parm                    dscmz
163200010201     C                   parm      *blanks       esito
163300010205     C                   if        %error
163400010205     C                             or cmzerr = '1'
163500010205     C                             or esito  = '1'
163600010205     C                   eval      wrkesito = '3'
163700010205     C                   else
163800010201     C*
163900060225     C* 3° invio FIVAB
164000010201     C                   reset                   dscmz
164100010201     C                   move      vlrpoi        cmzdst
164200010201     C                   eval      cmzfld = vlrfou
164300010201     C                   eval      cmzmbd = vlrhdl
164400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
164500021009     C***                if        prmfir = *blanks
164600021113     C                   eval      cmzfla = 'FIVAB00F'
164700021113     C                   eval      cmzmba = 'FIVAB00F'
164800021009     C***                else
164900021009     C***                eval      cmzfla = prmfir
165000021009     C***                eval      cmzmba = prmfir
165100021009     C***                endif
165200010201     C                   eval      cmznrr = *zeros
165300010201     C                   move      §ctrokvb      cmznrr
165400021018     C                   eval      cmzlba = vlrfl1
165500010201     C                   call(e)   'TIS711C'
165600010201     C                   parm                    dscmz
165700010201     C                   parm      *blanks       esito
165800010201     C                   if        %error
165900010201     C                             or cmzerr = '1'
166000010201     C                             or esito  = '1'
166100010201     C                   eval      wrkesito = '3'
166200010201     C                   endif
166300010205     C                   endif
166400060225     C                   endif
166500990920     C*
166600000613     C                   ENDSR
166700000613     C***
166800990910
166900000613     C     *inzsr        BEGSR
167000990910     C*
167100990910     C     *entry        plist
167200990920     C                   parm                    tivlrds
167300990921     C                   parm      wrkesito      esito
167400000724     C                   parm                    prmlit
167500000710     C                   parm                    prmfir
167600000613     C*
167700000830     C* CALCOLA LA DATA CORRENTE
167800000830     C                   time                    wn14             14 0
167900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
168000000830     C                   z-add     wn8           g08dat
168100000830     C                   z-add     *zeros        g08inv
168200000830     C                   movel     '0'           g08err
168300000830     C                   call      'XSRDA8'
168400000830     C                   parm                    wlbda8
168500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
168600041210     C*
168700041210     C* Chiave su TABEL00F - parziale
168800041210     C     KEYtabP       KLIST
168900041210     C                   KFLD                    tblKUT
169000041210     C                   KFLD                    tblCOD
169100000830     C*
169200000613     C                   ENDSR
169300000613     C***
169400990908
169500041206     Otitvg4p   E            riepilogo         2
169600990915     O                                              'Upload via Internet'
169700990915     O                                           +1 'Traduzione TIVIN00R -'
169800060225     O                                           +1 'FIVAB/FIVAT/FIVAX'
169900010201     O                                           +1 'Report testate bolle'
170000990915     O          E            riepilogo   2
170100990915     O                       wrkdata
170200990915     O                       wrkora              +1
170300990915     O                       procname            +1
170400990915     O          E            riepilogo   2
170500990915     O                                              'Cliente..................:'
170600990915     O                       VABCCM        z     +1
170700990915     O          E            riepilogo   2
170800990920     O                                              'Riferimento Strategi.....:'
170900990920     O                       vlrhdl              +1
171000990915     O          E            riepilogo   2
171100990915     O                                              'Giusti...................:'
171200010201     O                       §CTROKVB      2   +  1
171300990915     O          E            riepilogo   2
171400990915     O                                              'Sbagliati e corretti.....:'
171500971022     O                       §CTRMO        2   +  1
171600990915     O          E            riepilogo   2
171700990915     O                                              'Sbagliati e scartati.....:'
171800971022     O                       §CTRNO        2   +  1
171900000613
172000041206     Otitvg4ps  E            testdett          2
172100000613     O                                              'Upload via Internet'
172200000613     O                                           +1 'Traduzione TIVIN00R -'
172300060225     O                                           +1 'FIVAB/FIVAT/FIVAX'
172400010201     O                                           +1 'Report testate bolle'
172500000616     O          E            testdett    3
172600000613     O                                           +2 'N° rec'
172700000613     O                                           +3 'Anteprima contenuto'
172800000616     O          E            rigadett    2
172900000613     O                       rrnum               +2
173000000621     O                       recko               +3
173100000616     O          E            findett     2
173200000613     O                       wrkdata
173300000613     O                       wrkora              +1
173400000613     O                       procname            +1
173500000616     O          E            findett     2
173600000613     O                                              'Cliente..................:'
173700000613     O                       VABCCM        z     +1
173800000616     O          E            findett     2
173900000613     O                                              'Riferimento Strategi.....:'
174000000613     O                       vlrhdl              +1
174100000616     O          E            findett     2
174200000613     O                                              'Giusti...................:'
174300010201     O                       §CTROKVB      2   +  1
174400000616     O          E            findett     2
174500000613     O                                              'Sbagliati e corretti.....:'
174600000613     O                       §CTRMO        2   +  1
174700000616     O          E            findett     2
174800000613     O                                              'Sbagliati e scartati.....:'
174900000613     O                       §CTRNO        2   +  1
175000000512** CMD - COMANDI CL
175100041206OVRPRTF FILE(TITVG4P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
175200041206OVRPRTF FILE(TITVG4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
175300041206DLTOVR FILE(TITVG4P TITVG4PS) LVL(*)
175400000512
175500000512
