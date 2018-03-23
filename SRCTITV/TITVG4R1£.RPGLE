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
001200070502     F                                     oflind(*inoa)
001300070502     F                                     infsr(*pssr)
001400041206     Ftitvg4ps  O    f  198        PRINTER usropn
001500000621     F                                     oflind(*inob)
001600070502     F                                     infsr(*pssr)
001700000512     D*------------
001800000512     D* COMANDI
001900000512     D*------------
002000011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002100041210     D*------------
002200041210     D* SCHIERE
002300041210     D*------------
002400041210     D skNAZISO        S              3    DIM(1000)
002500041210     D skNAZBAR        S              3    DIM(1000)
002600060227     D skNAZBARDES     S             25    DIM(1000)
002700000801     D*----------------------------------------------------
002800000801     D* DICHIARAZIOINE VARIABILI DI WRK
002900000801     D*----------------------------------------------------
003000990920     D dscmz         e ds                  inz
003100990910     D psds           sds
003200990910     D  procname         *PROC
003300990920     D tivlrds       e ds                  extname(tivlr00f)
003400041210     D ds15          e ds
003500990910     D esito           s              1
003600000724     D prmlit          s             10
003700000710     D prmfir          s             10
003800990921     D wrkesito        s                   like(esito)
003900990915     D wrkdata         s               d
004000990915     D wrkora          s               t
004100000613     D rrnum           s              6  0 INZ(*zeros)
004200000621     D recko           s            150    INZ(*blanks)
004300011113     D depcmd          s            150    INZ(*blanks)
004400041210     D parccm          s              8    INZ(*blanks)
004500010202     D parmbr          s             10    INZ(*blanks)
004600010202     D paropz          s              1    INZ(*blanks)
004700010202     D chkcall         s              1    INZ(*blanks)
004800041210     D FlgNewBol       s              1    INZ(*blanks)
004900041210     D jNAZ            s              5  0 INZ(*zeros)
005000060313     D VATTRC_E        s                   LIKE(VATTRC) INZ(*blanks)
005100060313     D VATNOT_E        s                   LIKE(VATNOT) INZ(*blanks)
005200060313     D VATTRC_B        s                   LIKE(VATTRC) INZ(*blanks)
005300060313     D VATNOT_B        s                   LIKE(VATNOT) INZ(*blanks)
005400000830
005500000830     D*------------------
005600000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
005700000830     D*------------------
005800000830     D WLBDA8          DS                  INZ
005900000830     D  G08DAT                 1      8  0
006000000830     D  G08INV                 9     16  0
006100000830     D  G08ERR                17     17
006200000830     D  G08TGI                18     22  0
006300041025     D*------------------
006400041025     D* DS REPERIMENTO NUMERATORE
006500041025     D*------------------
006600041025     D trul33ds      e ds                  inz
006700041025     D*------------------
006800041025     D* DS ARCHITETTURA
006900041025     D*------------------
007000041025     D kpjba         e ds                  inz
007100041025     D*------------------
007200060225
007300060225     D*------------------
007400060225     D* DEFINIZIONE DS D WRK X CONFORMAZIONE LAYOUT PACKING LIST
007500060225     D*------------------
007600060227     D DS__T_1         DS                  INZ
007700060720     D  sT1_LOGO                     26
007800060720     D  sT1_MITT                     21
007900060720     D  sT1_BLK1                      5
008000060720     D  sT1_DEST                     59
008100060225     D*
008200060227     D DS__T_2         DS                  INZ
008300060313     D  sT2_TES1_1                   18
008400060313     D  sT2_TES1_2                   25
008500060314     D  sT2_BLK1                      3
008600060227     D  sT2_TES2_1                   20
008700060313     D  sT2_TES2_2                   50
008800060227     D*
008900060227     D DS__R_1         DS                  INZ
009000060227     D  sR1_SEP                     116    INZ(*all'_')
009100060227     D*
009200060227     D DS__R_2         DS                  INZ
009300060227     D  sR2_SEP                     116    INZ(*all'¯')
009400060227     D*
009500060227     D DS__R_3         DS                  INZ
009600060227     D  sR3_SEP                     116    INZ(*all'=')
009700060227     D*
009800060227     D DS__D_1         DS                  INZ
009900060227     D  sD1_CONST1                   16    INZ('Posizione')
010000060314     D  sD1_CONST2                   60    INZ('Codice materiale/Descrizione')
010100060313     D  sD1_CONST3                   12    INZ('Qtà spedita')
010200060314     D  sD1_CONST4                    2    INZ
010300060313     D  sD1_CONST5                   12    INZ('Qtà ordinata')
010400060313     D  sD1_CONST6                    3    INZ
010500060313     D  sD1_CONST7                   12    INZ('Qtà residua')
010600060227     D*
010700060313     D DS__D_3         DS                  INZ
010800060313     D**sD3_DESCR                   116    INZ('Spedizione effettuata da:')
010900060720     D**sD3_DESCR                   116    INZ('Merce a vettore:')
011000060720     D  sD3_DESCR                   116    INZ
011100060227     D*
011200060227     D DS__D_5         DS                  INZ
011300060228     D  sD5_DESCR                   116
011400060227     D*
011500060227     D DS__D_6         DS                  INZ
011600060227     D  sD6_POSNUM                   16
011700060314     D  sD6_MATNUM                   60
011800060313     D  sD6_QTA                      12
011900060314     D  sD6_BLK1                      2
012000060228     D  sD6_QTAORD                   12
012100060314     D  sD6_BLK2                      2
012200060228     D  sD6_QTARES                   12
012300060228     D*
012400060228     D DS__D_7         DS                  INZ
012500060228     D  sD7_BLK1                     16
012600060228     D  sD7_DETMATDES                40
012700060228     D*
012800060228     D DS__D_8         DS                  INZ
012900060315     D* sD8_BLK1                     16
013000060228     D  sD8_CONST1                   20    INZ('Nr. ordine cliente:')
013100060313     D* sD8_CLIORDNUM                35
013200060313     D  sD8_CLIORDNUM                20
013300060313     D  sD8_BLK2                      2
013400060228     D  sD8_CONST2                    5    INZ('del:')
013500060228     D  sD8_CLIORDDAT                10
013600060228     D*
013700060228     D DS__D_9         DS                  INZ
013800060228     D  sD9_BLK1                     16
013900060228     D  sD9_CONST1                   10    INZ('N. part:')
014000060228     D  sD9_DETBARCOD                10
014100060228     D  sD9_BLK2                      5
014200060228     D  sD9_CONST2                   12    INZ('Data scad:')
014300060314     D  sD9_DATSCAD                  20
014400060314     D  sD9_BLK3                      2
014500060314     D  sD9_QTASPED                  12
014600060227     D*
014700060227     D DS__E_1         DS                  INZ
014800060227     D  sE1_CONST1                  100    INZ('INFORMAZIONI SU RESIDUO DA EVAD-
014900060228     D                                     ERE')
015000060227     D*
015100060227     D DS__E_2         DS                  INZ
015200060227     D  sE2_CONST1                   12    INZ('Nr.Doc. ')
015300060227     D  sE2_CONST2                    7    INZ('Pos.')
015400060227     D  sE2_CONST3                   20    INZ('Materiale')
015500060314     D  sE2_CONST4                   42    INZ('Descrizione')
015600060314     D  sE2_BLK1                      5
015700060227     D  sE2_CONST5                   17    INZ('Qtà Ordinata')
015800060314     D  sE2_BLK2                      1
015900060314     D  sE2_CONST6                   17    INZ('Qtà Residua')
016000060227     D*
016100060227     D DS__E_3         DS                  INZ
016200060227     D  sE3_NUMORD                   12
016300060227     D  sE3_POS                       7
016400060227     D  sE3_MATNUM                   20
016500060227     D  sE3_MATDES                   42
016600060227     D  sE3_QTAORD                   17
016700060227     D  sE3_QTADIF                   17
016800060228     D*
016900060228     D DS__P_1         DS                  INZ
017000060228     D  sP1_DICITUR1                 15
017100060313     D  sP1_VALORE1                  12
017200060313     D  sP1_BLK1                      4
017300060313     D  sP1_DICITUR2                 15
017400060313     D  sP1_VALORE2                  12
017500060313     D  sP1_BLK2                      4
017600060313     D  sP1_DICITUR3                 15
017700060313     D  sP1_VALORE3                  12
017800060313     D  sP1_BLK3                      4
017900060313     D  sP1_DICITUR4                 15
018000060313     D  sP1_VALORE4                   6
018100060227     D*
018200060225
018300060225     D*------------------
018400060225     D* DEFINIZIONE DS D WRK X RIDEFINIZIONE TIPI RECORD FILE CLIENTE
018500060225     D*------------------
018600060225     D DS_A1           DS                  INZ
018700060225     D  dA1_NUMORD                   10
018800060225     D  dA1_POS                       6
018900060225     D  dA1_MATNUM                   18
019000060225     D  dA1_MATDES                   40
019100060225     D  dA1_QTAORD                   15
019200060225     D  dA1_QTADIF                   15
019300060225     D  dA1_UNIT                      3
019400060228     D*
019500060508     D wDS_A1          DS                  INZ
019600060228     D  wdA1_NUMORD                  10
019700060228     D  wdA1_POS                      6
019800060228     D  wdA1_MATNUM                  18
019900060228     D  wdA1_MATDES                  40
020000060228     D  wdA1_QTAORD                  15
020100060228     D  wdA1_QTADIF                  15
020200060228     D  wdA1_UNIT                     3
020300060225     D*
020400060225     D DS_A2           DS                  INZ
020500060225     D  dA2_CONTAT                   40
020600060225     D  dA2_TEL                      30
020700060225     D  dA2_FAX                      30
020800060225     D*
020900060225     D DS_A3           DS                  INZ
021000060225     D  dA3_MITTID                   17
021100060225     D  dA3_MITTRSD                  40
021200060225     D  dA3_MITTIND                  40
021300060225     D  dA3_MITTCAP                  10
021400060225     D  dA3_MITTLOC                  40
021500060225     D  dA3_MITTNAZ                   3
021600060225     D*
021700060225     D DS_A4           DS                  INZ
021800060227     D  dA4_DESTID                   17
021900060225     D  dA4_DESTRSD                  40
022000060225     D  dA4_DESTRD2                  40
022100060225     D  dA4_DESTIND                  40
022200060225     D  dA4_DESTCAP                  10
022300060225     D  dA4_DESTLOC                  40
022400060225     D  dA4_DESTNAZ                   3
022500060225     D  dA4_DESTPRV                   3
022600060225     D*
022700060225     D DS_A5           DS                  INZ
022800060225     D  dA5_ORIGRSD                  40
022900060225     D  dA5_ORIGIND                  40
023000060225     D  dA5_ORIGCIV                   4
023100060225     D  dA5_ORIGCAP                  10
023200060225     D  dA5_ORIGLOC                  40
023300060227     D  dA5_ORIGNAZ                   3
023400060228     D  dA5_ORIGNAZD                 25
023500060225     D*
023600060225     D DS_A6           DS                  INZ
023700060228     D  dA6_DATSPE                   10
023800060225     D*
023900060225     D DS_A7           DS                  INZ
024000060227     D  dA7_VETTRSD                  40
024100060227     D  dA7_VETTIND                  40
024200060227     D  dA7_VETTCAP                  10
024300060227     D  dA7_VETTLOC                  40
024400060227     D  dA7_VETTNAZ                   3
024500060225     D*
024600060225     D DS_A8           DS                  INZ
024700060225     D  dA8_NUMDOC                   10
024800060225     D  dA8_PORTO1                    3
024900060225     D  dA8_PORTO2                   28
025000060225     D  dA8_ROUTE                     6
025100060225     D  dA8_PESOTOT                  17
025200060225     D  dA8_PESONET                  15
025300060225     D  dA8_PESOUNIT                  3
025400060225     D  dA8_VOLTOT                   15
025500060225     D  dA8_VOLUNIT                   3
025600060225     D  dA8_COLLI                     5
025700060720     D*
025800060720     D DS_A9           DS                  INZ
025900060720     D  dA9_COMUSOMIT                40
026000060720     D  dA9_COMUSODES                40
026100060720     D  dA9_COMUSO001               132
026200060720     D  dA9_COMUSO002               132
026300060314     D*
026400060314     D DS_B99          DS                  INZ
026500060314     D  dB6_DETPOSNUM                 6
026600060314     D  dB6_DETMATNUM                18
026700060314     D  dB6_DETMATDES                40
026800060314     D  dB6_DETBARCOD                10
026900060314     D  dB6_DETQTASPE                15
027000060314     D  dB6_DETUNIT                   3
027100060314     D  dB1_QTADIF                   15
027200060314     D  dB1_UNIT                      3
027300060314     D  dB1_QTAORD                   15
027400060314     D  dB2_CLIORDNUM                35
027500060314     D  dB2_CLIORDDAT                10
027600060314     D  dB3_CLIORDNUM                35
027700060314     D  dB3_CLIORDDAT                10
027800060314     D  dB4_DATSCAD                  30
027900060314     D  dB4_1_DATSCAD                30
028000060314     D  dB5_IDOCNUM                  10
028100060225     D*
028200060314     D wDS_B99         DS                  INZ
028300060314     D wdB6_DETPOSNUM                 6
028400060314     D wdB6_DETMATNUM                18
028500060314     D wdB6_DETMATDES                40
028600060314     D wdB6_DETBARCOD                10
028700060314     D wdB6_DETQTASPE                15
028800060314     D wdB6_DETUNIT                   3
028900060314     D wdB1_QTADIF                   15
029000060314     D wdB1_UNIT                      3
029100060314     D wdB1_QTAORD                   15
029200060314     D wdB2_CLIORDNUM                35
029300060314     D wdB2_CLIORDDAT                10
029400060314     D wdB3_CLIORDNUM                35
029500060314     D wdB3_CLIORDDAT                10
029600060314     D wdB4_DATSCAD                  30
029700060314     D wdB4_1_DATSCAD                30
029800060314     D wdB5_IDOCNUM                  10
029900060314     D*
030000060225
030100060225     D*------------------
030200060225     D* DEFINIZIONE SCHIERE X CONFORMAZIONE LAYOUT PACKING LIST
030300060225     D*------------------
030400060314     D skVAR_PK        S                   DIM(200) LIKE(DS_B99) INZ(*blanks)   * schiera Packings
030500060314     D jVAR_PK         S              3  0 INZ(*zeros)                          * suo indice
030600060314     D skVAR_RS        S                   DIM(200) LIKE(DS_A1)  INZ(*blanks)   * schiera Residui
030700060227     D jVAR_RS         S              3  0 INZ(*zeros)                          * suo indice
030800990908
030900010201
031000010201
031100990915     C                   time                    wrkdata
031200990915     C                   time                    wrkora
031300000913     C                   reset                   rrnum
031400990921     C                   reset                   esito
031500990921     C                   reset                   wrkesito
031600000724     C*
031700000724     C* SE OCCORRE SPEDIRE IN FILIALE
031800000724     C                   if        vlrpoi <> *zeros
031900000724     C*
032000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
032100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
032200000724     C     vlrpoi        chain     azorg01l
032300041210     C                   if        %found(azorg01l)
032400000616     C                   movel(p)  CMD(1)        depcmd
032500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
032600000616     C*
032700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
032800011113     C                   Z-ADD     150           LENGH            15 5
032900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
033000000616     C                   PARM                    depcmd
033100000616     C                   PARM                    LENGH
033200000724     C*
033300000724     C                   endif
033400000724     C                   endif
033500000616     C*
033600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
033700000616     C                   movel(p)  CMD(2)        depcmd
033800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
033900000616     C*
034000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
034100011113     C                   Z-ADD     150           LENGH            15 5
034200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
034300000616     C                   PARM                    depcmd
034400000616     C                   PARM                    LENGH
034500000616     C*
034600041206     C                   if        not %open(titvg4ps)
034700041206     C                   open      titvg4ps
034800000616     C                   except    testdett
034900000613     C                   endif
035000000613     C*
035100041210     C                   EXSR      CARTAB                                       CARICA TABELLE
035200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
035300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
035400080930     C*
035500080930     C                   if        %open(tivin00r)
035600080930     C                   close     tivin00r
035700080930     C                   endif
035800080930     C                   if        %open(fivabwwr)
035900080930     C                   close     fivabwwr
036000080930     C                   endif
036100080930     C                   if        %open(fivatwwr)
036200080930     C                   close     fivatwwr
036300080930     C                   endif
036400080930     C                   if        %open(fivaxwwr)
036500080930     C                   close     fivaxwwr
036600080930     C                   endif
036700000613     C*
036800010202     C* Effettuo la chiamata al CLLE preposto
036900060225     C                   call(e)   'TITVVTXC'
037000010202     C                   parm                    parccm
037100010202     C                   parm                    parmbr
037200010202     C                   parm      '2'           paropz
037300010202     C*
037400041206     C                   if        %open(titvg4ps)
037500000616     C                   except    findett
037600041206     C                   close     titvg4ps
037700000613     C                   endif
037800000616     C*
037900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
038000011113     C                   Z-ADD     150           LENGH            15 5
038100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
038200000616     C                   PARM                    CMD(3)
038300000616     C                   PARM                    LENGH
038400000616     C*
038500010201     C                   seton                                        LR
038600000613
038700000613
038800000613     C*--------------------------------------------------------
038900000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
039000000613     C*--------------------------------------------------------
039100000621     C     STPR          BEGSR
039200000613     C*
039300041206     C                   if        not %open(titvg4p)
039400041206     C                   open      titvg4p
039500990915     C                   endif
039600990915     C*
039700990915     C                   except    riepilogo
039800990915     C*
039900041206     C                   if        %open(titvg4p)
040000041206     C                   close     titvg4p
040100990914     C                   endif
040200990910     C*
040300000613     C                   ENDSR
040400000613     C***
040500041210
040600041210     C*--------------------------------------------------------
040700041210     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
040800041210     C*--------------------------------------------------------
040900041210     C     CARTAB        BEGSR
041000041210     C*
041100041210     C* TABELLA '15' - NAZIONI
041200041210     C                   clear                   skNAZISO
041300041210     C                   clear                   skNAZBAR
041400060227     C                   clear                   skNAZBARDES
041500041210     C                   eval      tblKUT = 1
041600041210     C                   eval      tblCOD = '15'
041700041210     C     KEYtabP       setll     tabel00f
041800041210     C     KEYtabP       reade     tabel00f
041900041210     C                   dow       not %eof(tabel00f)
042000041210     C                   if        tblFLG = *blanks
042100041210     C                   movel(p)  tblUNI        ds15
042200041210     C                   add       1             jNAZ
042300041210     C                   eval      skNAZBAR(jNAZ) = tblKEY
042400060227     C                   eval      skNAZBARDES(jNAZ) = §15DES
042500041210     C                   eval      skNAZISO(jNAZ) = §15COD
042600041210     C                   endif
042700041210     C     KEYtabP       reade     tabel00f
042800041210     C                   enddo
042900041210     C*
043000041210     C                   ENDSR
043100041210     C***
043200041210
043300041210
043400041210
043500910830     C*--------------------------------------------------------
043600060225     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR e FIVAXWWR  *
043700910830     C*--------------------------------------------------------
043800040526     C     RWFILE        BEGSR
043900990910     C*
044000990914     C                   if        not %open(tivin00r)
044100990908     C                   open      tivin00r
044200990914     C                   endif
044300021113     C                   if        not %open(fivabwwr)
044400021113     C                   open      fivabwwr
044500990914     C                   endif
044600021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
044700060225     C                   exsr      prevatvax
044800010201     C*
044900010202     C                   if        chkcall = '0'
045000010202     C*
045100021113     C                   if        not %open(fivatwwr)
045200021113     C                   open      fivatwwr
045300010201     C                   endif
045400060225     C*
045500060225     C                   if        not %open(fivaxwwr)
045600060225     C                   open      fivaxwwr
045700060225     C                   endif
045800990910     C*
045900010201     C                   clear                   §CTROKVB          5 0
046000020305     C                   clear                   §CTROKVT          5 0
046100060225     C                   clear                   §CTROKVX          5 0
046200000801     C                   clear                   §CTRMO            5 0
046300000801     C                   clear                   §CTRNO            5 0
046400040910     C*
046500921023     C                   DO        *HIVAL
046600990913     C*
046700990915     C                   READ      tivin00r                               70
046800040910     C                   if        vindta > *blanks
046900000613     C                   add       1             rrnum
047000000801     C*
047100000801     C                   if        *in70 = *off
047200000801     C                             and
047300000801     C                             (vinflg = *blanks
047400000801     C                              or vinflg = '0'
047500000801     C                              or vinflg = '2')
047600000801     C*
047700000801     C                   clear                   vinmsg
047800000801     C                   eval      vinflg = '1'
047900040910     C*
048000040910     C* Eseguo routine d traduzione
048100060225     C                   exsr      impvbvtvx
048200040802     C*
048300010305     C                   endif
048400000905     C*
048500000905     C                   else
048600000905     C                   eval      vinflg = '1'
048700000905     C                   endif
048800000905     C*
048900000905     C  N70              update    tivin000
049000000905     C*
049100991022     C  N70              ENDdo
049200041210     C*
049300060227     C* Scarico i buffer eventualmente rimasti in sospeso
049400041210     C                   IF        FlgNewBol = '1'
049500060321     C*
049600060321     C* Scrivo solo i record relativi all'Itinerario V770 (ovvero Bartolini)
049700060321     C* l'Itinerario V880 è Radio Merci Rinaldi (x cui escludo)
049800060321     C                   IF        dA8_ROUTE = 'V770'
049900041210     C                   WRITE     FIVAB000
050000060313     C                   IF        VATNOT_B <> *blanks
050100060313     C                   EVAL      VATTRC = VATTRC_B
050200060313     C                   EVAL      VATNOT = VATNOT_B
050300041210     C                   WRITE     FIVAT000
050400041210     C                   ENDIF
050500060314     C* Memorizzo gli ultimi dati rimasti "in canna"
050600060314     C                   ADD       1             jVAR_PK
050700060314     C                   EVAL      skVAR_PK(jVAR_PK) = DS_B99
050800060227     C                   EXSR      WRIVAX
050900060321     C                   ENDIF
051000041210     C                   ENDIF
051100010202     C*
051200010202     C                   endif
051300990910
051400990910     C* Se non ci sono record con errori ...
051500000710     C                   if        §ctrno = 0
051600990910     C* ... restituisco esito OK.
051700990921     C                   eval      wrkesito = '0'
051800990910     C                   else
051900010201     C                   if        §ctrokvb > 0
052000990921     C                   eval      wrkesito = '1'
052100000710     C                   else
052200000710     C                   eval      wrkesito = '2'
052300990910     C                   endif
052400000710     C                   endif
052500990910     C*
052600010201     C                   if        §ctrokvb > 0
052700000724     C                             and vlrpoi <> *zeros
052800010202     C                   exsr      invio
052900990920     C                   endif
053000990920     C*
053100910830     C                   ENDSR
053200000613     C***
053300990920
053400000801     C*----------------------------------------------------*
053500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053600000801     C*----------------------------------------------------*
053700010201     C     INZVAR        BEGSR
053800000801     C*
053900040802     C                   Z-ADD     *zeros        Num5_0            5 0
054000040802     C                   MOVEL     '0'           FlgCAS            1
054100060313     C                   MOVEL     '0'           FlgDKC            1
054200060313     C                   Z-ADD     *zeros        wQtaDif          30 9
054300060313     C                   Z-ADD     *zeros        wQtaOrd          30 9
054400060313     C                   Z-ADD     *zeros        wQtaEvasa        30 3
054500060227     C*
054600060227     C* Inizializzo tutte le schiere e le ds d wrk
054700060227     C                   CLEAR                   DS_A1
054800060227     C                   CLEAR                   DS_A2
054900060227     C                   CLEAR                   DS_A3
055000060227     C                   CLEAR                   DS_A4
055100060227     C                   CLEAR                   DS_A5
055200060227     C                   CLEAR                   DS_A6
055300060228     C***                CLEAR                   DS_A7
055400060227     C                   CLEAR                   DS_A8
055500060314     C                   CLEAR                   DS_B99
055600060720     C                   CLEAR                   wComodUso         1
055700060227     C*
055800060314     C                   CLEAR                   skVAR_PK
055900060227     C                   Z-ADD     *zeros        jVAR_PK
056000060228     C                   CLEAR                   skVAR_RS
056100060227     C                   Z-ADD     *zeros        jVAR_RS
056200000801     C*
056300000801     C                   ENDSR
056400000801     C*----------------------------------------------------*
056500040910     C*  IMPOSTAZIONE CAMPI COSTANTI
056600000801     C*----------------------------------------------------*
056700000801     C     DEFCAM        BEGSR
056800000801     C*
056900021113     C                   CLEAR                   FIVAB000
057000040802     C                   CLEAR                   FIVAT000
057100060225     C                   CLEAR                   FIVAX000
057200020619     C* Imposto i valori di default...
057300041210     C                   Z-ADD     0431893       VABCCM
057400041210     C                   Z-ADD     0431893       VATCCM
057500060225     C                   Z-ADD     0431893       VAXCCM
057600041210     C                   Z-ADD     043           VABLNP
057700041210     C                   Z-ADD     043           VATLNP
057800060225     C                   Z-ADD     043           VAXLNP
057900041210     C                   Z-ADD     000           VABCTR
058000060228     C                   MOVEL     '7A'          VABCTM
058100040823     C                   MOVEL     '1'           VABCBO
058200020619     C* ... e poi verifico se sono stati passati come parametri
058300020619     C                   IF        vlrppt > *blanks
058400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
058500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
058600020619     C                   EXSR      CHKNUM
058700020619     C                   IF        PiInt=*on
058800020619     C                   Z-ADD     PiVal         VABCCM
058900020619     C                   Z-ADD     PiVal         VATCCM
059000060225     C                   Z-ADD     PiVal         VAXCCM
059100020619     C                   ENDIF
059200040506     C                   ENDIF
059300040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
059400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
059500020619     C                   EXSR      CHKNUM
059600020619     C                   IF        PiInt=*on
059700020619     C                   Z-ADD     PiVal         VABLNP
059800020619     C                   Z-ADD     PiVal         VATLNP
059900060225     C                   Z-ADD     PiVal         VAXLNP
060000040506     C                   ENDIF
060100020619     C                   ENDIF
060200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
060300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
060400020619     C                   EXSR      CHKNUM
060500020619     C                   IF        PiInt=*on
060600020619     C                   Z-ADD     PiVal         VABCTR
060700040506     C                   ENDIF
060800020619     C                   ENDIF
060900041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
061000041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
061100041210     C                   ENDIF
061200060313     C                   EVAL      FlgDKC = %subst(vlrppt:16:1)
061300020619     C                   ENDIF
061400000801     C*
061500000801     C                   ENDSR
061600000801     C*----------------------------------------------------*
061700060225     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT e FIVAX)
061800000801     C*----------------------------------------------------*
061900060225     C     IMPVBVTVX     BEGSR
062000041210     C*
062100041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
062200041210     C                   Z-ADD     *zeros        errore            1 0
062300060313     C*
062400060313     C********
062500060313     C* Tipo record 'E1EDL20' (dati testata: riferimenti, peso, volume, colli, etc..)
062600060313     C********
062700060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL20'
062800060314     C*
062900060314     C* Memorizzo i dati rimasti "in canna" dalla bolla precedente
063000060314     C                   ADD       1             jVAR_PK
063100060314     C                   EVAL      skVAR_PK(jVAR_PK) = DS_B99
063200060314     C                   CLEAR                   DS_B99
063300060313     C*
063400060313     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del FIVAB
063500060313     C                   IF        FlgNewBol = '1'
063600060321     C*
063700060321     C* Scrivo solo i record relativi all'Itinerario V770 (ovvero Bartolini)
063800060321     C* l'Itinerario V880 è Radio Merci Rinaldi (x cui escludo)
063900060321     C                   IF        dA8_ROUTE = 'V770'
064000060313     C                   WRITE     FIVAB000
064100060313     C                   IF        VATNOT_B <> *blanks
064200060313     C                   EVAL      VATTRC = VATTRC_B
064300060313     C                   EVAL      VATNOT = VATNOT_B
064400060313     C                   WRITE     FIVAT000
064500060313     C                   ENDIF
064600060313     C                   EXSR      WRIVAX
064700060321     C                   ENDIF
064800060313     C                   EVAL      FlgNewBol = '0'
064900060313     C                   ENDIF
065000060313     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
065100060313     C                   EXSR      INZVAR
065200060313     C                   EXSR      DEFCAM
065300060313     C                   EVAL      FlgNewBol = '1'
065400060313     C* Valorizzazione => DS_A8
065500060313     C                   EVALR     dA8_NUMDOC   = %trim(%subst(vindta:63+1:10))
065600060313     C                   MOVE(P)   dA8_NUMDOC    wNum10_0         10 0
065700060313     C                   EVALR     dA8_NUMDOC   = %trim(%editc(wNum10_0:'4'))
065800060313     C                   EVAL      dA8_PORTO1   = %trim(%subst(vindta:115+1:3))
065900060313     C                   EVAL      dA8_PORTO2   = %trim(%subst(vindta:118+1:28))
066000060313     C                   EVAL      dA8_ROUTE    = %trim(%subst(vindta:146+1:6))
066100060313     C                   EVALR     dA8_PESOTOT  = %trim(%subst(vindta:154+1:17))
066200060313     C                   EVAL      PiStr=dA8_PESOTOT
066300060313     C                   EXSR      CHKNUM
066400060313     C                   IF        PiNum=*on
066500060313     C                   IF        PiInt=*on
066600060313     C                   Z-ADD     PiVal         wNum30_0         30 0
066700060313     C                   EVALR     dA8_PESOTOT  = %trim(%editc(wNum30_0:'2'))
066800060313     C                   ELSE
066900060313     C                   Z-ADD     PiVal         wNum30_3         30 3
067000060313     C                   EVALR     dA8_PESOTOT  = %trim(%editc(wNum30_3:'2'))
067100060313     C                   ENDIF
067200060313     C                   ENDIF
067300060313     C                   EVALR     dA8_PESONET  = %trim(%subst(vindta:171+1:15))
067400060313     C                   EVAL      PiStr=dA8_PESONET
067500060313     C                   EXSR      CHKNUM
067600060313     C                   IF        PiNum=*on
067700060313     C                   IF        PiInt=*on
067800060313     C                   Z-ADD     PiVal         wNum30_0         30 0
067900060313     C                   EVALR     dA8_PESONET  = %trim(%editc(wNum30_0:'2'))
068000060313     C                   ELSE
068100060313     C                   Z-ADD     PiVal         wNum30_3         30 3
068200060313     C                   EVALR     dA8_PESONET  = %trim(%editc(wNum30_3:'2'))
068300060313     C                   ENDIF
068400060313     C                   ENDIF
068500060313     C                   EVAL      dA8_PESOUNIT = %trim(%subst(vindta:186+1:3))
068600060313     C                   EVALR     dA8_VOLTOT   = %trim(%subst(vindta:189+1:15))
068700060313     C                   EVAL      PiStr=dA8_VOLTOT
068800060313     C                   EXSR      CHKNUM
068900060313     C                   IF        PiNum=*on
069000060313     C                   IF        PiInt=*on
069100060313     C                   Z-ADD     PiVal         wNum30_0         30 0
069200060313     C                   EVALR     dA8_VOLTOT   = %trim(%editc(wNum30_0:'2'))
069300060313     C                   ELSE
069400060313     C                   Z-ADD     PiVal         wNum30_3         30 3
069500060313     C                   EVALR     dA8_VOLTOT   = %trim(%editc(wNum30_3:'2'))
069600060313     C                   ENDIF
069700060313     C                   ENDIF
069800060313     C                   EVAL      dA8_VOLUNIT  = %trim(%subst(vindta:204+1:3))
069900060313     C                   EVALR     dA8_COLLI    = %trim(%subst(vindta:207+1:5))
070000060313     C                   MOVE(P)   dA8_COLLI     wNum5_0           5 0
070100060313     C                   EVALR     dA8_COLLI    = %trim(%editc(wNum5_0:'2'))
070200060313     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
070300060313     C                   MOVEL     datcor        VABAAS
070400060313     C                   MOVEL     datcor        VATAAS
070500060313     C                   MOVEL     datcor        VAXAAS
070600060313     C                   MOVE      datcor        VABMGS
070700060313     C                   MOVE(P)   vlrpoi        VABFGS
070800060313     C                   MOVE(P)   vlrpoi        VATFGS
070900060313     C                   MOVE(P)   vlrpoi        VAXFGS
071000060313     C* ......VABNSP/VATNSP/VAXNSP => Stacco un numeratore da AZNUM
071100060313     C                   CLEAR                   TRUL33DS
071200060313     C                   EVAL      I33OPE = *zeros
071300060313     C                   EVAL      I33CNU = 302
071400060313     C                   EVAL      I33NUM = 1
071500060313     C                   MOVEL     TRUL33DS      KPJBU
071600060313     C                   CALL      'TRUL33R'
071700060313     C                   PARM                    KPJBA
071800060313     C                   MOVEL     KPJBU         TRUL33DS
071900060313     C                   IF        O33ERR = *zeros
072000060313     C                   Z-ADD     O33NRF        VABNSP
072100060313     C                   Z-ADD     O33NRF        VATNSP
072200060313     C                   Z-ADD     O33NRF        VAXNSP
072300060313     C                   ELSE
072400060313     C                   ADD       1             errore
072500060313     C                   EVAL      vinmsg = %trimr(vinmsg)
072600060313     C                             + ' ' + 'VABNSP VATNSP VAXNSP'
072700060313     C                   ENDIF
072800060313     C* ......VABRMN
072900060313     C                   EVAL      PiStr=%trim(%subst(vindta:64:10))
073000060313     C                   EXSR      CHKNUM
073100060313     C                   IF        PiInt=*on
073200060313     C                   Z-ADD     PiVal         VABRMN
073300060313     C                   ELSE
073400060313     C                   Z-ADD     1             VABRMN
073500060313     C                   ADD       1             errore
073600060313     C                   EVAL      vinmsg = %trimr(vinmsg)
073700060313     C                             + ' ' + 'VABRMN'
073800060313     C                   ENDIF
073900060313     C* ......VABRMA
074000060313     C                   EVAL      VABRMA=%trim(%subst(vindta:64:10))
074100060313     C* ......VABPKB
074200060313     C                   EVAL      PiStr=%trim(%subst(vindta:155:17))
074300060313     C                   EXSR      CHKNUM
074400060313     C                   IF        PiNum=*on
074500060313     C                   IF        %trim(%subst(vindta:187:3)) = 'GRM'          * da grammi a kili
074600060313     C                   EVAL      PiVal=PiVal/1000
074700060313     C                   Z-ADD     PiVal         VABPKB
074800060313     C                   ELSE
074900060313     C                   Z-ADD     PiVal         VABPKB                        * altrimenti dft. kg
075000060313     C                   ENDIF
075100060313     C                   ELSE
075200060313     C                   ADD       1             errore
075300060313     C                   EVAL      vinmsg = %trimr(vinmsg)
075400060313     C                             + ' ' + 'VABPKB'
075500060313     C                   ENDIF
075600060313     C* ......VABVLB
075700060313     C                   EVAL      PiStr=%trim(%subst(vindta:190:15))
075800060313     C                   EXSR      CHKNUM
075900060313     C                   IF        PiNum=*on
076000060313     C                   IF        %trim(%subst(vindta:205:3)) = 'DMQ'          * da dec3 a m3
076100060313     C                   EVAL      PiVal=PiVal/1000
076200060313     C                   Z-ADD     PiVal         VABVLB
076300060313     C                   ELSE
076400060313     C                   Z-ADD     PiVal         VABVLB                         * altrimenti dft. m3
076500060313     C                   ENDIF
076600060313     C                   ELSE
076700060313     C                   ADD       1             errore
076800060313     C                   EVAL      vinmsg = %trimr(vinmsg)
076900060313     C                             + ' ' + 'VABVLB'
077000060313     C                   ENDIF
077100060313     C* ......VABNCL
077200060313     C                   EVAL      PiStr=%trim(%subst(vindta:208:5))
077300060313     C                   EXSR      CHKNUM
077400060313     C                   IF        PiInt=*on
077500060313     C                   Z-ADD     PiVal         VABNCL
077600060313     C                   ELSE
077700060313     C                   ADD       1             errore
077800060313     C                   EVAL      vinmsg = %trimr(vinmsg)
077900060313     C                             + ' ' + 'VABNCL'
078000060313     C                   ENDIF
078100060313     C*
078200060313     C                   ENDIF
078300060227     C*
078400060227     C********
078500060227     C* Tipo record 'Z1EDL14'
078600060227     C********
078700060227     C                   IF        %trim(%subst(vindta:1:10)) = 'Z1EDL14'
078800060227     C* Valorizzazione => DS_A1
078900060228     C                   EVAL      dA1_NUMORD   = %trim(%subst(vindta:63+1:10))
079000060228     C                   MOVE(P)   dA1_NUMORD    wNum10_0         10 0
079100060228     C                   EVALR     dA1_NUMORD   = %trim(%editc(wNum10_0:'4'))
079200060227     C                   EVAL      dA1_POS      = %trim(%subst(vindta:73+1:6))
079300060509     C***                EVALR     dA1_MATNUM   = %trim(%subst(vindta:79+1:18))
079400060509     C***                MOVE(P)   dA1_MATNUM    wNum18_0         18 0
079500060509     C***                EVALR     dA1_MATNUM   = %trim(%editc(wNum18_0:'4'))
079600060509     C                   EVAL      dA1_MATNUM   = %trim(%subst(vindta:79+1:18))
079700060227     C                   EVAL      dA1_MATDES   = %trim(%subst(vindta:97+1:40))
079800060228     C                   EVALR     dA1_QTAORD   = %trim(%subst(vindta:137+1:15))
079900060227     C                   EVAL      PiStr=dA1_QTAORD
080000060227     C                   EXSR      CHKNUM
080100060228     C                   IF        PiNum=*on
080200060228     C                   IF        PiInt=*on
080300060228     C                   Z-ADD     PiVal         wNum30_0         30 0
080400060228     C                   EVALR     dA1_QTAORD   = %trim(%editc(wNum30_0:'2'))
080500060228     C                   ELSE
080600060228     C                   Z-ADD     PiVal         wNum30_3         30 3
080700060228     C                   EVALR     dA1_QTAORD   = %trim(%editc(wNum30_3:'2'))
080800060228     C                   ENDIF
080900060228     C                   ENDIF
081000060228     C                   EVALR     dA1_QTADIF   = %trim(%subst(vindta:152+1:15))
081100060227     C                   EVAL      PiStr=dA1_QTADIF
081200060227     C                   EXSR      CHKNUM
081300060228     C                   IF        PiNum=*on
081400060228     C                   IF        PiInt=*on
081500060228     C                   Z-ADD     PiVal         wNum30_0         30 0
081600060228     C                   EVALR     dA1_QTADIF   = %trim(%editc(wNum30_0:'2'))
081700060228     C                   ELSE
081800060228     C                   Z-ADD     PiVal         wNum30_3         30 3
081900060228     C                   EVALR     dA1_QTADIF   = %trim(%editc(wNum30_3:'2'))
082000060228     C                   ENDIF
082100060228     C                   ENDIF
082200060227     C                   EVAL      dA1_UNIT     = %trim(%subst(vindta:167+1:3))
082300060227     C*
082400060227     C* Trattandosi d record "ripetuto" memorizzo in specifica schiera
082500060228     C                   ADD       1             jVAR_RS
082600060228     C                   EVAL      skVAR_RS(jVAR_RS) = DS_A1
082700060227     C                   ENDIF
082800060227     C*
082900060227     C********
083000060227     C* Tipo record 'Z1EDL20'
083100060227     C********
083200060227     C                   IF        %trim(%subst(vindta:1:10)) = 'Z1EDL20'
083300060227     C* Valorizzazione => DS_A2
083400060227     C                   EVAL      dA2_CONTAT   = %trim(%subst(vindta:130+1:40))
083500060227     C                   EVAL      dA2_TEL      = %trim(%subst(vindta:170+1:30))
083600060313     C* Tolgo il prefisso internazionale x l'Italia
083700070102     C                   IF        %len(%trim(dA2_TEL)) >= 5 AND
083800070102     C                             %subst(%trim(dA2_TEL):1:5) = '0039/'
083900060313     C                   EVAL      dA2_TEL      = %subst(%trim(dA2_TEL):6)
084000060313     C                   ENDIF
084100060313     C                   EVAL      dA2_FAX      = %trim(%subst(vindta:200+1:30))
084200060313     C* Tolgo il prefisso internazionale x l'Italia
084300070102     C                   IF        %len(%trim(dA2_FAX)) >= 5 AND
084400070102     C                             %subst(%trim(dA2_FAX):1:5) = '0039/'
084500060313     C                   EVAL      dA2_FAX      = %subst(%trim(dA2_FAX):6)
084600060313     C                   ENDIF
084700060227     C                   ENDIF
084800060227     C*
084900060227     C********
085000060313     C* Tipo record 'E1ADRM1'
085100060227     C* - soto tipo 'AG' => Mittente
085200060227     C********
085300060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM1' AND
085400060227     C                             %trim(%subst(vindta:64:3)) = 'AG'
085500060227     C* Valorizzazione => DS_A3
085600060228     C                   EVALR     dA3_MITTID   = %trim(%subst(vindta:67+1:17))
085700060227     C                   MOVE(P)   dA3_MITTID    wNum17_0         17 0
085800060228     C                   EVALR     dA3_MITTID   = %trim(%editc(wNum17_0:'4'))
085900060227     C                   EVAL      dA3_MITTRSD  = %trim(%subst(vindta:118+1:40))
086000060227     C                   EVAL      dA3_MITTIND  = %trim(%subst(vindta:438+1:40))
086100060227     C                   EVAL      dA3_MITTCAP  = %trim(%subst(vindta:572+1:10))
086200060227     C                   EVAL      dA3_MITTLOC  = %trim(%subst(vindta:607+1:40))
086300060227     C                   EVAL      dA3_MITTNAZ  = %trim(%subst(vindta:937+1:3))
086400060227     C                   ENDIF
086500060313     C*
086600060313     C********
086700060313     C* Tipo record 'E1ADRM1' (dati instradamento: rag. soc. dest, indirizzo dest, etc...)
086800060313     C* - soto tipo 'WE' => Destinatario Merce
086900060313     C********
087000060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM1' AND
087100060313     C                             %trim(%subst(vindta:64:3)) = 'WE'
087200060313     C* Valorizzazione => DS_A4
087300060313     C                   EVALR     dA4_DESTID   = %trim(%subst(vindta:67+1:17))
087400060313     C                   MOVE(P)   dA4_DESTID    wNum17_0         17 0
087500060313     C                   EVALR     dA4_DESTID   = %trim(%editc(wNum17_0:'4'))
087600060720     C                   EVAL      dA4_DESTRSD  = %trim(%subst(vindta:118+1:40))
087700060313     C                   EVAL      dA4_DESTRD2  = %trim(%subst(vindta:158+1:40))
087800060313     C                   EVAL      dA4_DESTIND  = %trim(%subst(vindta:438+1:40))
087900060313     C                   EVAL      dA4_DESTCAP  = %trim(%subst(vindta:572+1:10))
088000060313     C                   EVAL      dA4_DESTLOC  = %trim(%subst(vindta:607+1:40))
088100060313     C                   EVAL      dA4_DESTNAZ  = %trim(%subst(vindta:937+1:3))
088200060313     C                   EVAL      dA4_DESTPRV  = %trim(%subst(vindta:943+1:3))
088300060313     C*
088400060313     C* ......VABRSD/VABRD2
088500060313     C                   MOVEL     *blanks       wRSD             70
088600060313     C                   EVAL      wRSD = %trim(%subst(vindta:119:40)) + ' ' +
088700060313     C                                    %trim(%subst(vindta:159:40)) + ' ' +
088800060313     C                                    %trim(%subst(vindta:199:40)) + ' ' +
088900060313     C                                    %trim(%subst(vindta:239:40))
089000060313     C                   EVAL      VABRSD=%subst(wRSD:1:35)
089100060313     C                   EVAL      VABRD2=%subst(wRSD:1+35:35)
089200060313     C* ......VABIND
089300060313     C                   EVAL      VABIND=%trim(%subst(vindta:439:40)) + ' ' +
089400060313     C                                    %trim(%subst(vindta:479:40)) + ' ' +
089500060313     C                                    %trim(%subst(vindta:519:40)) + ' ' +
089600060313     C                                    %trim(%subst(vindta:559:40))
089700060313     C* ......VABCAD
089800060313     C                   EVAL      PiStr=%trim(%subst(vindta:573:10))
089900060313     C                   EXSR      CHKNUM
090000060313     C                   IF        PiInt=*on
090100060313     C                   Z-ADD     PiVal         Num5_0
090200060313     C                   MOVEL(P)  Num5_0        VABCAD
090300060313     C                   ELSE
090400060313     C                   ADD       1             errore
090500060313     C                   EVAL      vinmsg = %trimr(vinmsg)
090600060313     C                             + ' ' + 'VABCAD'
090700060313     C                   ENDIF
090800060313     C* ......VABLOD
090900060313     C                   EVAL      VABLOD=%trim(%subst(vindta:608:40)) + ' ' +
091000060313     C                                    %trim(%subst(vindta:648:40))
091100060313     C* ......VATNOT + Tipo record "B"
091200060313     C                   EVAL      VATNOT_B=%trim(%subst(vindta:748:30)) + ' ' +
091300060313     C                                      %trim(%subst(vindta:778:40))
091400060313     C                   EVAL      VATTRC_B = 'B'
091500060313     C* ......VABNZD (conversone da formato *ISO a formato "BARTOLINI"
091600060313     C                   EVAL      VABNZD=%trim(%subst(vindta:938:3))
091700060313     C                   Z-ADD     1             jNAZ
091800060313     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
091900060313     C                   IF        %found
092000060313     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
092100060313     C                   ENDIF
092200060313     C* ......VABPRD
092300060313     C                   EVAL      VABPRD=%trim(%subst(vindta:944:3))
092400060313     C*
092500060313     C                   ENDIF
092600060720     C*
092700060720     C********
092800060720     C* Tipo record 'E1TXTH8'
092900060720     C* - soto tipo 'VBBK' => (trattasi d spedizione "tipo" comodato uso/conto deposito)
093000060720     C********
093100060720     C                   IF        %trim(%subst(vindta:1:10)) = 'E1TXTH8' AND
093200060720     C                             %trim(%subst(vindta:66+1:10)) = 'VBBK'
093300060720     C* Imposto il flag che pilota questo particolare comportamento
093400060720     C                   EVAL      wComodUso = 'S'
093500060720     C* Valorizzazione => DS_A9 - parte fissa
093600060720     C                   EVAL      dA9_COMUSOMIT='Spedizione per conto di:'
093700060720     C                   EVAL      dA9_COMUSODES='Da consegnare a:'
093800060720     C                   EVAL      dA4_DESTRD2  = dA4_DESTRSD
093900060720     C                   EVAL      dA4_DESTRSD  = 'B.Braun Milano S.p.A c/o'
094000060720     C*
094100060720     C                   ENDIF
094200060720     C********
094300060720     C* Tipo record 'E1TXTP8' (dati relativi alle spedizioni "tipo" comodato uso/conto deposito)
094400060720     C********
094500060720     C                   IF        %trim(%subst(vindta:1:10)) = 'E1TXTP8'
094600060720     C* Valorizzazione => DS_A9 - parte variabile
094700060720     C                   MOVEL     dA9_COMUSO002 wCOMUSO003      132
094800060720     C                   MOVEL     dA9_COMUSO002 dA9_COMUSO001
094900060720     C                   EVAL      dA9_COMUSO002=%trim(%subst(vindta:65+1:132))
095000060720     C*
095100060720     C                   ENDIF
095200060227     C*
095300060227     C********
095400060227     C* Tipo record 'E1ADRM1'
095500060227     C* - soto tipo 'OSO' => Mittente originale
095600060227     C********
095700060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM1' AND
095800060227     C                             %trim(%subst(vindta:64:3)) = 'OSO'
095900060227     C* Valorizzazione => DS_A5
096000060227     C                   EVAL      dA5_ORIGRSD  = %trim(%subst(vindta:118+1:40))
096100060227     C                   EVAL      dA5_ORIGIND  = %trim(%subst(vindta:438+1:40))
096200060227     C                   EVAL      dA5_ORIGCIV  = %trim(%subst(vindta:558+1:4))
096300060227     C                   EVAL      dA5_ORIGCAP  = %trim(%subst(vindta:572+1:10))
096400060227     C                   EVAL      dA5_ORIGLOC  = %trim(%subst(vindta:607+1:40))
096500060227     C                   EVAL      dA5_ORIGNAZ  = %trim(%subst(vindta:937+1:3))
096600060227     C                   Z-ADD     1             jNAZ
096700060227     C     dA5_ORIGNAZ   LOOKUP    skNAZISO(jNAZ)                         55
096800060227     C                   IF        %found
096900060227     C                   EVAL      dA5_ORIGNAZD = skNAZBARDES(jNAZ)
097000060227     C                   ENDIF
097100060227     C                   ENDIF
097200060227     C*
097300060227     C********
097400060227     C* Tipo record 'E1EDT13'
097500060227     C* - soto tipo '007' => IDoc Delivery
097600060227     C********
097700060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDT13' AND
097800060227     C                             %trim(%subst(vindta:64:3)) = '007'
097900060227     C* Valorizzazione => DS_A6
098000060227     C                   EVAL      dA6_DATSPE   = %trim(%subst(vindta:90+1:8))
098100060227     C                   EVAL      dA6_DATSPE   = %subst(dA6_DATSPE:7:2)+ '.'+
098200060227     C                                            %subst(dA6_DATSPE:5:2)+ '.'+
098300060227     C                                            %subst(dA6_DATSPE:1:4)
098400060313     C                   ENDIF
098500060313     C*
098600060313     C********
098700060314     C* Tipo record 'E1EDL24' Informazioni elementi dettagli articoli/ordine spediz. corrente
098800060313     C********
098900060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL24'
099000060314     C*
099100060314     C* Trattandosi d record "ripetuto" memorizzo in specifica schiera e inizializzo la DS
099200060314     C                   ADD       1             jVAR_PK
099300060314     C                   EVAL      skVAR_PK(jVAR_PK) = DS_B99
099400060314     C                   CLEAR                   DS_B99
099500060314     C* Valorizzazione => DS_B6 (di cui DS_B99)
099600060313     C                   IF        %trim(%subst(vindta:336+1:6)) <> *blanks
099700060313     C                   EVAL      dB6_DETPOSNUM= %trim(%subst(vindta:336+1:6))
099800060313     C                   ELSE
099900060313     C                   EVAL      dB6_DETPOSNUM= %trim(%subst(vindta:63+1:6))
100000060313     C                   ENDIF
100100060509     C***                EVALR     dB6_DETMATNUM= %trim(%subst(vindta:69+1:18))
100200060509     C***                MOVE(P)   dB6_DETMATNUM wNum18_0         18 0
100300060509     C***                EVALR     dB6_DETMATNUM= %trim(%editc(wNum18_0:'4'))
100400060509     C                   EVAL      dB6_DETMATNUM= %trim(%subst(vindta:69+1:18))
100500060313     C                   EVAL      dB6_DETMATDES= %trim(%subst(vindta:105+1:40))
100600060313     C                   EVAL      dB6_DETBARCOD= %trim(%subst(vindta:207+1:10))
100700060313     C                   EVALR     dB6_DETQTASPE= %trim(%subst(vindta:239+1:15))
100800060313     C                   EVAL      PiStr=dB6_DETQTASPE
100900060313     C                   EXSR      CHKNUM
101000060313     C                   IF        PiNum=*on
101100060313     C                   IF        PiInt=*on
101200060313     C                   Z-ADD     PiVal         wNum30_0         30 0
101300060313     C                   EVALR     dB6_DETQTASPE= %trim(%editc(wNum30_0:'2'))
101400060313     C                   ELSE
101500060313     C                   Z-ADD     PiVal         wNum30_3         30 3
101600060313     C                   EVALR     dB6_DETQTASPE= %trim(%editc(wNum30_3:'2'))
101700060313     C                   ENDIF
101800060313     C                   ENDIF
101900060313     C                   EVAL      dB6_DETUNIT  = %trim(%subst(vindta:254+1:3))
102000060313     C                   ENDIF
102100060313     C*
102200060313     C********
102300060313     C* Tipo record 'Z1EDL24'
102400060313     C********
102500060313     C                   IF        %trim(%subst(vindta:1:10)) = 'Z1EDL24'
102600060314     C* Valorizzazione => DS_B1 (di cui DS_B99)
102700060313     C                   EVALR     dB1_QTADIF   = %trim(%subst(vindta:164+1:15))
102800060313     C                   EVAL      PiStr=dB1_QTADIF
102900060313     C                   EXSR      CHKNUM
103000060313     C                   IF        PiNum=*on
103100060313     C                   IF        PiInt=*on
103200060313     C                   Z-ADD     PiVal         wNum30_0         30 0
103300060313     C                   EVALR     dB1_QTADIF   = %trim(%editc(wNum30_0:'2'))
103400060313     C                   ELSE
103500060313     C                   Z-ADD     PiVal         wNum30_3         30 3
103600060313     C                   EVALR     dB1_QTADIF   = %trim(%editc(wNum30_3:'2'))
103700060313     C                   ENDIF
103800060313     C                   ENDIF
103900060313     C                   Z-ADD     PiVal         wQtaDif
104000060313     C                   EVAL      dB1_UNIT     = %trim(%subst(vindta:179+1:3))
104100060313     C                   EVALR     dB1_QTAORD   = %trim(%subst(vindta:208+1:15))
104200060313     C                   EVAL      PiStr=dB1_QTAORD
104300060313     C                   EXSR      CHKNUM
104400060313     C                   IF        PiNum=*on
104500060313     C                   IF        PiInt=*on
104600060313     C                   Z-ADD     PiVal         wNum30_0         30 0
104700060313     C                   EVALR     dB1_QTAORD   = %trim(%editc(wNum30_0:'2'))
104800060313     C                   ELSE
104900060313     C                   Z-ADD     PiVal         wNum30_3         30 3
105000060313     C                   EVALR     dB1_QTAORD   = %trim(%editc(wNum30_3:'2'))
105100060313     C                   ENDIF
105200060313     C                   Z-ADD     PiVal         wQtaOrd
105300060313     C                   ENDIF
105400060313     C                   ENDIF
105500060313     C*
105600060313     C********
105700060313     C* Tipo record 'E1EDL15'
105800060313     C* - soto tipo '9999999400' => IDoc Caratteristiche
105900060313     C********
106000060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL15' AND
106100060313     C                             %trim(%subst(vindta:64:10)) = '9999999400'
106200060314     C* Valorizzazione => DS_B4 (di cui DS_B99)
106300060313     C                   EVAL      dB4_DATSCAD  = %trim(%subst(vindta:133+1:30))
106400060313     C                   ENDIF
106500060313     C*
106600060313     C********
106700060313     C* Tipo record 'E1EDL15'
106800060313     C* - soto tipo '0000000823' => IDoc Caratteristiche
106900060313     C********
107000060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL15' AND
107100060313     C                             %trim(%subst(vindta:64:10)) = '0000000823'
107200060314     C* Valorizzazione => DS_B4_1 (di cui DS_B99)
107300060313     C                   EVAL      dB4_1_DATSCAD= %trim(%subst(vindta:133+1:30))
107400060313     C                   ENDIF
107500060313     C*
107600060313     C********
107700060313     C* Tipo record 'E1EDL43'
107800060313     C* - soto tipo 'C' => IDoc Riferimenti
107900060313     C********
108000060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL43' AND
108100060313     C                             %trim(%subst(vindta:64:1)) = 'C'
108200060314     C* Valorizzazione => DS_B5 (di cui DS_B99)
108300060313     C***                EVAL      dB5_IDOCNUM  = %trim(%subst(vindta:64+1:35))
108400060313     C                   EVAL      dB5_IDOCNUM  = %trim(%subst(vindta:64+1:10))
108500060313     C                   MOVE(P)   dB5_IDOCNUM   wNum10_0         10 0
108600060313     C                   EVALR     dB5_IDOCNUM  = %trim(%editc(wNum10_0:'4'))
108700060313     C                   ENDIF
108800060313     C*
108900060313     C********
109000060313     C* Tipo record 'E1EDL41'
109100060313     C* - soto tipo '001' => Riferimenti e date ordini - cliente
109200060313     C********
109300060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL41' AND
109400060313     C                             %trim(%subst(vindta:64:3)) = '001'
109500060314     C* Valorizzazione => DS_B2 (di cui DS_B99)
109600060313     C                   EVAL      dB2_CLIORDNUM= %trim(%subst(vindta:66+1:35))
109700060313     C                   EVAL      dB2_CLIORDDAT= %trim(%subst(vindta:101+1:8))
109800060313     C                   EVAL      dB2_CLIORDDAT= %subst(dB2_CLIORDDAT:7:2)+'.'+
109900060313     C                                            %subst(dB2_CLIORDDAT:5:2)+'.'+
110000060313     C                                            %subst(dB2_CLIORDDAT:1:4)
110100060313     C                   ENDIF
110200060313     C*
110300060313     C********
110400060313     C* Tipo record 'E1EDL41'
110500060313     C* - soto tipo '002' => Riferimenti e date ordini - evasione mittente
110600060313     C********
110700060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL41' AND
110800060313     C                             %trim(%subst(vindta:64:3)) = '002'
110900060314     C* Valorizzazione => DS_B3 (di cui DS_B99)
111000060313     C                   EVAL      dB3_CLIORDNUM= %trim(%subst(vindta:66+1:35))
111100060313     C                   EVAL      dB3_CLIORDDAT= %trim(%subst(vindta:101+1:8))
111200060313     C                   EVAL      dB3_CLIORDDAT= %subst(dB3_CLIORDDAT:7:2)+'.'+
111300060313     C                                            %subst(dB3_CLIORDDAT:5:2)+'.'+
111400060313     C                                            %subst(dB3_CLIORDDAT:1:4)
111500060313     C                   ENDIF
111600060227     C*
111700060227     C********
111800060313     C* Tipo record 'E1ADRM4'
111900060227     C* - soto tipo 'SP' => Vettore
112000060227     C********
112100060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM4' AND
112200060227     C                             %trim(%subst(vindta:64:3)) = 'SP'
112300060227     C* Valorizzazione => DS_A7
112400060227     C                   EVAL      dA7_VETTRSD  = %trim(%subst(vindta:118+1:40))
112500060227     C                   EVAL      dA7_VETTIND  = %trim(%subst(vindta:438+1:40))
112600060227     C                   EVAL      dA7_VETTCAP  = %trim(%subst(vindta:572+1:10))
112700060227     C                   EVAL      dA7_VETTLOC  = %trim(%subst(vindta:607+1:40))
112800060227     C                   EVAL      dA7_VETTNAZ  = %trim(%subst(vindta:937+1:3))
112900060227     C                   ENDIF
113000060313     C*
113100060313     C********
113200060313     C* Tipo record 'E1EDL37' Informazioni "CHI SONO" colli
113300060313     C********
113400060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL37'
113500060407     C                   EVAL      VATNOT_E = %trim(%subst(vindta:63+1:20))
113600060313     C                   EVAL      VATTRC_E = 'E'
113700060508     C                   IF        dA8_ROUTE = 'V770'
113800060508     C                   IF        FlgDKC   =  'S'
113900060508     C                   IF        VATNOT_E <> *blanks
114000060508     C                   EVAL      VATTRC = VATTRC_E
114100060508     C                   EVAL      VATNOT = VATNOT_E
114200060508     C                   WRITE     FIVAT000
114300060508     C                   ENDIF
114400060508     C                   ENDIF
114500060508     C                   ENDIF
114600060313     C                   ENDIF
114700040910     C*
114800040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
114900040910     C                   IF        FlgCAS <> '0'
115000040929     C                   IF        VABCBO = '1'
115100040910     C                   EVAL      VABCBO = '4'
115200040910     C                   ELSE
115300040929     C                   EVAL      VABCBO = '6'
115400040910     C                   ENDIF
115500040929     C                   ENDIF
115600040910     C*
115700040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
115800040910     C                   EXSR      CHKIMPDIV
115900010202     C*
116000000801     C* Ebbene...
116100000801     C                   ADD       1             §CTRMO
116200010201     C                   IF        errore <> *zeros
116300000801     C                   ADD       1             §CTRNO
116400000801     C                   EVAL      recko = vindta
116500000801     C                   EXCEPT    rigadett
116600000801     C                   EVAL      vinflg = '2'
116700000801     C                   ELSE
116800010201     C                   ADD       1             §CTROKVB
116900000801     C                   ENDIF
117000000801     C*
117100000801     C                   ENDSR
117200010202     C*----------------------------------------------------*
117300060225     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR E FILE FIVAXWWR
117400010202     C*----------------------------------------------------*
117500060225     C     PREVATVAX     BEGSR
117600010202     C*
117700060225     C* Compongo il nome del membro da dare al FIVATWWR e al FIVAXWWR
117800010202     C                   eval      parmbr = vlrhdl
117900010202     C                   movel     'M'           parmbr
118000041210     C                   eval      parccm = vlrksc
118100010202     C                   eval      paropz = '1'
118200010202     C* Effettuo la chiamata al CLLE preposto
118300060225     C                   call(e)   'TITVVTXC'
118400010202     C                   parm                    parccm
118500010202     C                   parm                    parmbr
118600010202     C                   parm                    paropz
118700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
118800010202     C                   if        %error
118900010202     C                   movel     '1'           chkcall
119000010202     C                   else
119100010202     C                   movel     '0'           chkcall
119200010202     C                   endif
119300010202     C*
119400010202     C                   ENDSR
119500000801     C*----------------------------------------------------*
119600000801     C*  CONTROLLO NUMERICITA' CAMPI
119700000801     C*----------------------------------------------------*
119800000801     C     CHKNUM        BEGSR
119900000801     C*
120000000801     C                   call(e)   'ISNUMERIC'
120100000801     C                   PARM                    PiStr            30
120200041210     C                   PARM      '.'           PiDecChr          1
120300000801     C                   PARM      *ZEROS        PiVal            30 9
120400000801     C                   PARM      '0'           PiInt             1
120500000801     C                   PARM      '0'           PiNum             1
120600000801     C                   IF        %error
120700060227     C                   EVAL      PiNum=*off
120800000801     C                   ENDIF
120900000801     C*
121000000801     C                   ENDSR
121100000801     C***
121200000801
121300011113
121400011113     C*----------------------------------------------------*
121500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
121600011113     C*----------------------------------------------------*
121700011113     C     CHKIMPDIV     BEGSR
121800011113     C*
121900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
122000011113     C                   Z-ADD     *zeros        wrkDec            9 9
122100011113     C*
122200011113     C* Come prima cosa effettuo considerazioni sulla divisa
122300011113     C                   IF        vabIAS > *zeros
122400011113     C                   IF        vabVAS <> 'EUR'
122500011113     C                   EVAL      vabVAS =  'ITL'
122600011113     C                   ENDIF
122700011113     C                   ENDIF
122800011113     C*
122900011113     C                   IF        vabCAS > *zeros
123000011113     C                   IF        vabVCA <> 'EUR'
123100011113     C                   EVAL      vabVCA =  'ITL'
123200011113     C                   ENDIF
123300011113     C                   ENDIF
123400011113     C*
123500011113     C                   IF        vabVMD > *zeros
123600020305     C                   IF        vabVAD <> 'EUR'
123700011113     C                   EVAL      vabVAD =  'ITL'
123800011113     C                   ENDIF
123900011113     C                   ENDIF
124000011113     C*
124100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
124200011113     C                   Z-ADD     vabIAS        wrkDec
124300011113     C                   IF        wrkDec > *zeros
124400011113     C                   IF        vabVAS = 'ITL'
124500011113     C                   EVAL      vabIAS = *zeros
124600011113     C                   ENDIF
124700011113     C                   ENDIF
124800011113     C*
124900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
125000011113     C                   Z-ADD     vabCAS        wrkDec
125100011113     C                   IF        wrkDec > *zeros
125200011113     C                   IF        vabVCA = 'ITL'
125300011113     C                   EVAL      vabCAS = *zeros
125400011113     C                   ENDIF
125500011113     C                   ENDIF
125600011113     C*
125700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
125800011113     C                   Z-ADD     vabVMD        wrkDec
125900011113     C                   IF        wrkDec > *zeros
126000011113     C                   IF        vabVAD = 'ITL'
126100011113     C                   EVAL      vabVMD = *zeros
126200011113     C                   ENDIF
126300011113     C                   ENDIF
126400011113     C*
126500011113     C                   ENDSR
126600011113     C***
126700011113
126800060227
126900060227
127000060227     C*----------------------------------------------------*
127100060227     C*  ROUTINE GENERAZIONE BUFFER FORMATI FILE FIVAXWWR
127200060227     C*----------------------------------------------------*
127300060227     C     WRIVAX        BEGSR
127400060313     C*
127500060313     C* Inizializzo deposito per verifica cambio posizioni articoli/ordini
127600060313     C                   MOVEL     *blanks       wCurrPosiz        6
127700060227     C*
127800060227     C* Inizializzo contatore d riga e flag fine dati DDT x bolla corrente
127900060227     C                   Z-ADD     1             VAXPRR
128000060227     C*
128100060227     C* Imposto il tipo modulo x la stampa del logo personalizzato
128200060227     C                   EVAL      VAXTMD = 'BBRA'
128300060227     C*
128400060227     C* Innanzitutto inizializzo il campo dati flat
128500060227     C                   CLEAR                   VAXDTT
128600060228     C*
128700060228     C* Imposto il carattere in "grassetto"
128800060228     C                   EVAL      VAXTRK = 'T1'
128900060314     C*
129000060314     C* Valorizzo subito la ds che contiene anche alcuni dati d testata con uno
129100060314     C* qualunque (tengo l'ultimo memorizzato) dei dettagli articoli/ordine spedizione corrente
129200060314     C                   EVAL      wDS_B99 = skVAR_PK(jVAR_PK)
129300060720     C*
129400060720     C* Se trattasi d spedizione "tipo" comodato uso/conto deposito => gestisco particolarità
129500060720     C                   IF        wComodUso = 'S'
129600060720     C                   RESET                   DS__T_1
129700060720     C                   EVAL      sT1_MITT = %trim(dA9_COMUSOMIT)
129800060720     C                   EVAL      sT1_DEST = %trim(dA9_COMUSODES)
129900060720     C                   EVAL      VAXDTT   = DS__T_1
130000060720     C                   WRITE     FIVAX000
130100060720     C                   ADD       1             VAXPRR
130200060720     C                   ENDIF
130300060227     C*
130400060227     C                   RESET                   DS__T_1
130500060313     C                   EVAL      sT1_MITT = %trim(dA3_MITTRSD)
130600060227     C                   EVAL      sT1_DEST = %trim(dA4_DESTRSD)
130700060227     C                   EVAL      VAXDTT   = DS__T_1
130800060228     C                   WRITE     FIVAX000
130900060228     C                   ADD       1             VAXPRR
131000060227     C*
131100060227     C                   RESET                   DS__T_1
131200060313     C                   EVAL      sT1_MITT = %trim(dA3_MITTIND)
131300060227     C                   EVAL      sT1_DEST = %trim(dA4_DESTRD2)
131400060227     C                   EVAL      VAXDTT   = DS__T_1
131500060228     C                   WRITE     FIVAX000
131600060228     C                   ADD       1             VAXPRR
131700060227     C*
131800060227     C                   RESET                   DS__T_1
131900060313     C                   EVAL      sT1_MITT = %trim(dA3_MITTCAP) + ' ' +
132000060313     C                                        %trim(dA3_MITTLOC)
132100060227     C                   EVAL      sT1_DEST = %trim(dA4_DESTIND)
132200060227     C                   EVAL      VAXDTT   = DS__T_1
132300060228     C                   WRITE     FIVAX000
132400060228     C                   ADD       1             VAXPRR
132500060227     C*
132600060227     C                   RESET                   DS__T_1
132700060313     C                   EVAL      sT1_DEST = %trim(dA4_DESTCAP) + ' '  +
132800060227     C                                        %trim(dA4_DESTLOC) + ' ' +
132900060314     C                                        '(' + %trim(dA4_DESTPRV) + ')'
133000060314     C***                                     %trim(dA4_DESTNAZ)
133100060227     C                   EVAL      VAXDTT   = DS__T_1
133200060228     C                   WRITE     FIVAX000
133300060228     C                   ADD       1             VAXPRR
133400060227     C*
133500060228     C* Imposto il carattere in "normale"
133600060228     C                   EVAL      VAXTRK = 'C1'
133700060228     C*
133800060227     C                   EVAL      VAXDTT   = *blanks
133900060228     C                   WRITE     FIVAX000
134000060228     C                   ADD       1             VAXPRR
134100060227     C*
134200060227     C                   RESET                   DS__T_2
134300060227     C                   EVAL      sT2_TES1_1 = 'Nr.Documento:'
134400060227     C                   EVAL      sT2_TES1_2 = %trim(dA8_NUMDOC)
134500060227     C                   EVAL      sT2_TES2_1 = 'Nr.Ordine vendita:'
134600060313     C***                EVAL      sT2_TES2_2 = %trim(dA1_NUMORD)
134700060314     C                   EVAL      sT2_TES2_2 = %trim(wdB5_IDOCNUM)
134800060227     C                   EVAL      VAXDTT   = DS__T_2
134900060228     C                   WRITE     FIVAX000
135000060228     C                   ADD       1             VAXPRR
135100060227     C*
135200060227     C                   RESET                   DS__T_2
135300060227     C                   EVAL      sT2_TES1_1 = 'Data spedizione:'
135400060227     C                   EVAL      sT2_TES1_2 = %trim(dA6_DATSPE)
135500060315     C****               EVAL      sT2_TES2_1 = 'Codice BBMI:'
135600060315     C****               EVAL      sT2_TES2_2 = %trim(dA4_DESTID)
135700060315     C                   EVAL      sT2_TES2_1 = 'Nr.Ord. acq. com.:'
135800060315     C                   EVAL      sT2_TES2_2 = %trim(wdB2_CLIORDNUM)+' del: '+
135900060315     C                                          %trim(wdB2_CLIORDDAT)
136000060227     C                   EVAL      VAXDTT   = DS__T_2
136100060228     C                   WRITE     FIVAX000
136200060228     C                   ADD       1             VAXPRR
136300060227     C*
136400060315     C                   RESET                   DS__T_2
136500060315     C                   EVAL      sT2_TES1_1 = 'Itinerario:'
136600060315     C                   EVAL      sT2_TES1_2 = %trim(dA8_ROUTE)
136700060315     C****               EVAL      sT2_TES2_1 = 'Committente:'
136800060315     C****               EVAL      sT2_TES2_2 = %trim(dA3_MITTID)
136900060315     C                   EVAL      sT2_TES2_1 = 'Referente:'
137000060315     C                   EVAL      sT2_TES2_2 = %trim(dA2_CONTAT)+' Tel '+
137100060315     C                                          %trim(dA2_TEL)   +' Fax '+
137200060315     C                                          %trim(dA2_FAX)
137300060227     C                   EVAL      VAXDTT   = DS__T_2
137400060228     C                   WRITE     FIVAX000
137500060228     C                   ADD       1             VAXPRR
137600060227     C*
137700060227     C                   RESET                   DS__T_2
137800060313     C                   EVAL      sT2_TES1_1 = 'Tipo porto:'
137900060313     C                   EVAL      sT2_TES1_2 = %trim(dA8_PORTO1) + ' '+
138000060313     C                                          %trim(dA8_PORTO2)
138100060227     C                   EVAL      VAXDTT   = DS__T_2
138200060228     C                   WRITE     FIVAX000
138300060228     C                   ADD       1             VAXPRR
138400060227     C*
138500060227     C                   EVAL      VAXDTT   = *blanks
138600060228     C                   WRITE     FIVAX000
138700060228     C                   ADD       1             VAXPRR
138800060313     C*
138900060313     C                   RESET                   DS__D_3
139000060313     C***                EVAL      sD3_DESCR  = %trim(sD3_DESCR)   + ' '    +
139100060313     C***                                       %trim(dA7_VETTRSD) + ' - '  +
139200060313     C***                                       %trim(dA7_VETTIND) + ' - '  +
139300060313     C***                                       %trim(dA7_VETTCAP) + ' '    +
139400060313     C***                                       %trim(dA7_VETTLOC)
139500060720     C                   EVAL      sD3_DESCR  = 'Merce a vettore: '            +
139600060313     C                                          'Bartolini S.p.A. - Via Monte '+
139700060313     C                                          'Pastello, 30 - 37057 San '    +
139800060315     C                                          'Giovanni Lupatoto (VR) - IT '
139900060313     C                   EVAL      VAXDTT   = DS__D_3
140000060313     C                   WRITE     FIVAX000
140100060313     C                   ADD       1             VAXPRR
140200060720     C*
140300060720     C* Se trattasi d spedizione "tipo" comodato uso/conto deposito => gestisco particolarità
140400060720     C                   IF        wComodUso = 'S'
140500060720     C* Riga vuota
140600060720     C                   EVAL      VAXDTT   = *blanks
140700060720     C                   WRITE     FIVAX000
140800060720     C                   ADD       1             VAXPRR
140900060720     C*
141000060720     C* Imposto il carattere in "normale + grassetto"
141100060720     C                   EVAL      VAXTRK = 'T2'
141200060720     C* Riga nota 001
141300060720     C                   RESET                   DS__D_3
141400060720     C                   EVAL      sD3_DESCR  = dA9_COMUSO001
141500060720     C                   EVAL      VAXDTT   = DS__D_3
141600060720     C                   WRITE     FIVAX000
141700060720     C                   ADD       1             VAXPRR
141800060720     C* Riga nota 002
141900060720     C                   RESET                   DS__D_3
142000060720     C                   EVAL      sD3_DESCR  = dA9_COMUSO002
142100060720     C                   EVAL      VAXDTT   = DS__D_3
142200060720     C                   WRITE     FIVAX000
142300060720     C                   ADD       1             VAXPRR
142400060720     C*
142500060720     C* Imposto il carattere in "normale"
142600060720     C                   EVAL      VAXTRK = 'C1'
142700060720     C                   ENDIF
142800060313     C*
142900060313     C                   EVAL      VAXDTT   = *blanks
143000060313     C                   WRITE     FIVAX000
143100060313     C                   ADD       1             VAXPRR
143200060313     C*
143300060313     C* Imposto il carattere in "normale + grassetto"
143400060313     C                   EVAL      VAXTRK = 'T2'
143500060313     C*
143600060227     C                   RESET                   DS__D_1
143700060227     C                   EVAL      VAXDTT   = DS__D_1
143800060228     C                   WRITE     FIVAX000
143900060228     C                   ADD       1             VAXPRR
144000060313     C*
144100060313     C* Imposto il carattere in "normale"
144200060313     C                   EVAL      VAXTRK = 'C1'
144300060315     C*
144400060315     C* ...riga DETT:  Numero e data ordine
144500060315     C                   RESET                   DS__D_8
144600060315     C                   EVAL      sD8_CLIORDNUM=%trim(wdB3_CLIORDNUM)
144700060315     C                   EVAL      sD8_CLIORDDAT=%trim(wdB3_CLIORDDAT)
144800060315     C                   EVAL      VAXDTT   = DS__D_8
144900060315     C                   WRITE     FIVAX000
145000060315     C                   ADD       1             VAXPRR
145100060228     C*
145200060228     C                   RESET                   DS__R_1
145300060228     C                   EVAL      VAXDTT   = DS__R_1
145400060228     C                   WRITE     FIVAX000
145500060228     C                   ADD       1             VAXPRR
145600060227     C*
145700060315     C**                 EVAL      VAXDTT   = *blanks
145800060315     C**                 WRITE     FIVAX000
145900060315     C**                 ADD       1             VAXPRR
146000060228     C*
146100060228     C* Se presenti informazioni sui dettagli Packing List => gestisco....
146200060228     C                   IF        jVAR_PK > *zeros
146300060314     C*
146400060314     C* Ordino la schiera dei dettagli articoli/ordine spedizione corrente
146500060314     C                   SORTA     skVAR_PK
146600060314     C*
146700060314     C* ...ciclo su tutti i dettagli (<n> righe)
146800060314     C                   Z-ADD     1             jWRK              3 0
146900060314     C                   DOW       jWRK <= %elem(skVAR_PK)
147000060228     C*
147100060228     C* Finchè memorizzato qualcosa...  altrimenti esco dal ciclo
147200060314     C                   IF        skVAR_PK(jWRK) <> *blanks
147300060313     C*
147400060313     C* Valorizzo la ds dei dettagli articoli/ordine
147500060314     C                   EVAL      wDS_B99 = skVAR_PK(jWRK)
147600060313     C*
147700060313     C* Stampo le seguenti 3 righe solo a cambio d posizione articoli/ordine
147800060314     C                   IF        %trim(wCurrPosiz) <> %trim(wdB6_DETPOSNUM)
147900060313     C*
148000060313     C* Salvo la posizione articolo/ordine corrente
148100060313     C                   EVAL      wCurrPosiz = %trim(wdB6_DETPOSNUM)
148200060313     C*
148300060313     C* Imposto il carattere in "normale + grassetto"
148400060314     C*
148500060314     C* ...riga DETT:  Posizione e numero ordine
148600060313     C                   EVAL      VAXTRK = 'T2'
148700060313     C                   RESET                   DS__D_6
148800060313     C                   EVAL      sD6_POSNUM = %trim(wdB6_DETPOSNUM)
148900060313     C                   EVAL      sD6_MATNUM = %trim(wdB6_DETMATNUM)
149000060314     C                   EVALR     sD6_QTAORD = %trim(wdB1_QTAORD) + ' ' +
149100060314     C                                          %trim(wdB1_UNIT)
149200060314     C                   IF        wdB1_QTADIF = *blanks
149300060314     C                   EVAL      wdB1_QTADIF = '0'
149400060313     C                   ENDIF
149500060314     C                   EVALR     sD6_QTARES = %trim(wdB1_QTADIF) + ' ' +
149600060314     C                                          %trim(wdB1_UNIT)
149700060313     C*
149800060313     C                   EVAL      VAXDTT   = DS__D_6
149900060313     C                   WRITE     FIVAX000
150000060313     C                   ADD       1             VAXPRR
150100060313     C*
150200060314     C* ...riga DETT:  Descrizione articolo
150300060313     C                   RESET                   DS__D_7
150400060313     C                   EVAL      sD7_DETMATDES=%trim(wdB6_DETMATDES)
150500060313     C                   EVAL      VAXDTT   = DS__D_7
150600060313     C                   WRITE     FIVAX000
150700060313     C                   ADD       1             VAXPRR
150800060313     C*
150900060313     C* Imposto il carattere in "normale"
151000060313     C                   EVAL      VAXTRK = 'C1'
151100060314     C*
151200060314     C                   ENDIF
151300060314     C*
151400060314     C* ...riga DETT:  Numero parte e data scadenza
151500060228     C                   RESET                   DS__D_9
151600060228     C                   EVAL      sD9_DETBARCOD=%trim(wdB6_DETBARCOD)
151700060314     C                   IF        wdB4_1_DATSCAD <> *blanks
151800060314     C                   EVAL      sD9_DATSCAD  =%trim(wdB4_1_DATSCAD)
151900060314     C                   ELSE
152000060314     C                   EVAL      sD9_DATSCAD  =%trim(wdB4_DATSCAD)
152100060314     C                   ENDIF
152200060314     C                   EVALR     sD9_QTASPED  =%trim(wdB6_DETQTASPE) + ' ' +
152300060314     C                                           %trim(wdB6_DETUNIT)
152400060228     C                   EVAL      VAXDTT   = DS__D_9
152500060313     C*
152600060313     C* Se presenti dati => stampo
152700060314     C                   IF        %trim(sD9_DETBARCOD) <> *blanks OR
152800060314     C                             %trim(sD9_DATSCAD)   <> *blanks
152900060228     C*
153000060228     C* Quindi scarico il buffer della riga corrente del DDT bolla corrente
153100060228     C                   WRITE     FIVAX000
153200060228     C*
153300060228     C* Incremento il contatore d riga corrente
153400060228     C                   ADD       1             VAXPRR
153500060313     C                   ENDIF
153600060314     C*
153700060314     C                   ENDIF
153800060314     C*
153900060314     C* Incremento l'indice della schiera corrente
154000060314     C                   ADD       1             jWRK
154100060314     C*
154200060228     C                   ENDDO
154300060228     C                   ENDIF
154400060228     C*
154500060228     C                   EVAL      VAXDTT   = *blanks
154600060228     C                   WRITE     FIVAX000
154700060228     C                   ADD       1             VAXPRR
154800060227     C*
154900060227     C* Se presenti informazioni sul "Residuo da evadere" => gestisco....
155000060227     C                   IF        jVAR_RS > *zeros
155100060228     C* ...titolo  (riga 26)
155200060227     C                   RESET                   DS__E_1
155300060227     C                   EVAL      VAXDTT   = DS__E_1
155400060228     C                   EVAL      VAXTRK = 'T1'                                * vado in grassetto
155500060227     C                   WRITE     FIVAX000
155600060228     C                   ADD       1             VAXPRR
155700060228     C                   EVAL      VAXTRK = 'C1'                                * torno a normale
155800060228     C* ...intestazioni colonne (riga 27)
155900060227     C                   RESET                   DS__E_2
156000060227     C                   EVAL      VAXDTT   = DS__E_2
156100060227     C                   WRITE     FIVAX000
156200060228     C                   ADD       1             VAXPRR
156300060228     C* ...riga separazione "alta" (riga 28)
156400060228     C                   RESET                   DS__R_1
156500060228     C                   EVAL      VAXDTT   = DS__R_1
156600060227     C                   WRITE     FIVAX000
156700060228     C                   ADD       1             VAXPRR
156800060314     C* ...ciclo su dettaglio articoli/ordini da evadere (<n> righe)
156900060228     C                   Z-ADD     *zeros        jWRK              3 0
157000060228     C                   DO        *hival
157100060228     C*
157200060228     C* Incremento l'indice della schiera corrente
157300060228     C                   ADD       1             jWRK
157400060228     C*
157500060228     C* Finchè memorizzato qualcosa...  altrimenti esco dal ciclo
157600060228     C                   IF        skVAR_RS(jWRK) <> *blanks
157700060228     C                   EVAL      wDS_A1 = skVAR_RS(jWRK)
157800060228     C                   RESET                   DS__E_3
157900060228     C                   EVAL      sE3_NUMORD = %trim(wdA1_NUMORD)
158000060228     C                   EVAL      sE3_POS    = %trim(wdA1_POS)
158100060228     C                   EVAL      sE3_MATNUM = %trim(wdA1_MATNUM)
158200060228     C                   EVAL      sE3_MATDES = %trim(wdA1_MATDES)
158300060314     C                   EVALR     sE3_QTAORD = %trim(wdA1_QTAORD) + ' ' +
158400060228     C                                          %trim(wdA1_UNIT)
158500060314     C                   EVALR     sE3_QTADIF = %trim(wdA1_QTADIF) + ' ' +
158600060228     C                                          %trim(wdA1_UNIT)
158700060228     C                   EVAL      VAXDTT   = DS__E_3
158800060228     C*
158900060228     C* Quindi scarico il buffer della riga corrente del DDT bolla corrente
159000060228     C                   WRITE     FIVAX000
159100060228     C*
159200060228     C* Incremento il contatore d riga corrente
159300060228     C                   ADD       1             VAXPRR
159400060228     C*
159500060228     C                   ELSE
159600060228     C*
159700060228     C* Al primo elemento d schiera nn valorizzato => esco dal ciclo...
159800060228     C                   LEAVE
159900060228     C                   ENDIF
160000060227     C*
160100060227     C                   ENDDO
160200060228     C                   ENDIF
160300060313     C*
160400060228     C                   RESET                   DS__R_1
160500060228     C                   EVAL      VAXDTT   = DS__R_1
160600060228     C                   WRITE     FIVAX000
160700060228     C                   ADD       1             VAXPRR
160800060313     C*
160900060313     C* Imposto il carattere in "normale + grassetto"
161000060313     C                   EVAL      VAXTRK = 'T2'
161100060313     C*
161200060313     C                   RESET                   DS__P_1
161300060228     C                   EVAL      sP1_DICITUR1='Peso totale:'
161400060228     C                   EVAL      sP1_VALORE1 =%trim(dA8_PESOTOT) + ' ' +
161500060228     C                                          %trim(dA8_PESOUNIT)
161600060313     C                   EVAL      sP1_DICITUR2='Peso netto:'
161700060313     C                   EVAL      sP1_VALORE2 =%trim(dA8_PESONET) + ' ' +
161800060313     C                                          %trim(dA8_PESOUNIT)
161900060313     C                   EVAL      sP1_DICITUR3='Volume totale:'
162000060313     C                   EVAL      sP1_VALORE3 =%trim(dA8_VOLTOT) + ' ' +
162100060313     C                                          %trim(dA8_VOLUNIT)
162200060313     C                   EVAL      sP1_DICITUR4='Numero colli:'
162300060313     C                   EVAL      sP1_VALORE4 =%trim(dA8_COLLI)
162400060228     C                   EVAL      VAXDTT   = DS__P_1
162500060228     C                   WRITE     FIVAX000
162600060228     C                   ADD       1             VAXPRR
162700060313     C*
162800060313     C* Imposto il carattere in "normale"
162900060313     C                   EVAL      VAXTRK = 'C1'
163000060228     C*
163100060228     C                   RESET                   DS__R_1
163200060228     C                   EVAL      VAXDTT   = DS__R_1
163300060228     C                   WRITE     FIVAX000
163400060228     C                   ADD       1             VAXPRR
163500060227     C*
163600060227     C                   ENDSR
163700011113
163800000801
163900000801
164000990920      /TITLE Invio dei dati al punto operativo.
164100010202     C     invio         BEGSR
164200060225     C*
164300060225     C* 1° invio FIVAX
164400060225     C                   reset                   dscmz
164500060225     C                   move      vlrpoi        cmzdst
164600060225     C                   eval      cmzfld = 'FIVAXWWR'
164700060225     C                   eval      cmzmbd = vlrhdl
164800060225     C                   eval      %subst(cmzmbd:1:1) = 'M'
164900060225     C***                if        prmfir = *blanks
165000060225     C                   eval      cmzfla = 'FIVAX00F'
165100060225     C                   eval      cmzmba = 'FIVAX00F'
165200060225     C***                else
165300060225     C***                eval      cmzfla = prmfir
165400060225     C***                eval      cmzmba = prmfir
165500060225     C***                endif
165600060225     C                   eval      cmznrr = *zeros
165700060227     C                   move      §ctrokvx      cmznrr
165800060225     C                   eval      cmzlba = vlrfl1
165900060225     C                   call(e)   'TIS711C'
166000060225     C                   parm                    dscmz
166100060225     C                   parm      *blanks       esito
166200060225     C                   if        %error
166300060225     C                             or cmzerr = '1'
166400060225     C                             or esito  = '1'
166500060225     C                   eval      wrkesito = '3'
166600060225     C                   else
166700990920     C*
166800060225     C* 2° invio FIVAT
166900010201     C                   reset                   dscmz
167000010201     C                   move      vlrpoi        cmzdst
167100021113     C                   eval      cmzfld = 'FIVATWWR'
167200010201     C                   eval      cmzmbd = vlrhdl
167300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
167400021009     C***                if        prmfir = *blanks
167500021113     C                   eval      cmzfla = 'FIVAT00F'
167600021113     C                   eval      cmzmba = 'FIVAT00F'
167700021009     C***                else
167800021009     C***                eval      cmzfla = prmfir
167900021009     C***                eval      cmzmba = prmfir
168000021009     C***                endif
168100010201     C                   eval      cmznrr = *zeros
168200020305     C                   move      §ctrokvt      cmznrr
168300021018     C                   eval      cmzlba = vlrfl1
168400010201     C                   call(e)   'TIS711C'
168500010201     C                   parm                    dscmz
168600010201     C                   parm      *blanks       esito
168700010205     C                   if        %error
168800010205     C                             or cmzerr = '1'
168900010205     C                             or esito  = '1'
169000010205     C                   eval      wrkesito = '3'
169100010205     C                   else
169200010201     C*
169300060225     C* 3° invio FIVAB
169400010201     C                   reset                   dscmz
169500010201     C                   move      vlrpoi        cmzdst
169600010201     C                   eval      cmzfld = vlrfou
169700010201     C                   eval      cmzmbd = vlrhdl
169800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
169900021009     C***                if        prmfir = *blanks
170000021113     C                   eval      cmzfla = 'FIVAB00F'
170100021113     C                   eval      cmzmba = 'FIVAB00F'
170200021009     C***                else
170300021009     C***                eval      cmzfla = prmfir
170400021009     C***                eval      cmzmba = prmfir
170500021009     C***                endif
170600010201     C                   eval      cmznrr = *zeros
170700010201     C                   move      §ctrokvb      cmznrr
170800021018     C                   eval      cmzlba = vlrfl1
170900010201     C                   call(e)   'TIS711C'
171000010201     C                   parm                    dscmz
171100010201     C                   parm      *blanks       esito
171200010201     C                   if        %error
171300010201     C                             or cmzerr = '1'
171400010201     C                             or esito  = '1'
171500010201     C                   eval      wrkesito = '3'
171600010201     C                   endif
171700010205     C                   endif
171800060225     C                   endif
171900990920     C*
172000000613     C                   ENDSR
172100000613     C***
172200070502
172300070502
172400070502
172500070502     C     *pssr         BEGSR
172600070502     C*
172700080930     C                   if        %open(tivin00r)
172800080930     C                   close     tivin00r
172900080930     C                   endif
173000070502     C                   if        %open(fivabwwr)
173100070502     C                   close     fivabwwr
173200070502     C                   endif
173300070502     C                   if        %open(fivatwwr)
173400070502     C                   close     fivatwwr
173500070502     C                   endif
173600070502     C                   if        %open(fivaxwwr)
173700070502     C                   close     fivaxwwr
173800070502     C                   endif
173900070502     C*
174000070502     C* Effettuo la chiamata al CLLE preposto
174100070502     C                   call(e)   'TITVVTXC'
174200070502     C                   parm                    parccm
174300070502     C                   parm                    parmbr
174400070502     C                   parm      '2'           paropz
174500070502     C*
174600070502     C                   eval      wrkesito = '2'
174700070502     C*
174800070502     C                   seton                                        LR
174900070502     C*
175000070502     C                   ENDSR     '*CANCL'
175100070502     C***
175200080930
175300080930
175400990910
175500000613     C     *inzsr        BEGSR
175600990910     C*
175700990910     C     *entry        plist
175800990920     C                   parm                    tivlrds
175900990921     C                   parm      wrkesito      esito
176000000724     C                   parm                    prmlit
176100000710     C                   parm                    prmfir
176200000613     C*
176300000830     C* CALCOLA LA DATA CORRENTE
176400000830     C                   time                    wn14             14 0
176500000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
176600000830     C                   z-add     wn8           g08dat
176700000830     C                   z-add     *zeros        g08inv
176800000830     C                   movel     '0'           g08err
176900000830     C                   call      'XSRDA8'
177000000830     C                   parm                    wlbda8
177100000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
177200041210     C*
177300041210     C* Chiave su TABEL00F - parziale
177400041210     C     KEYtabP       KLIST
177500041210     C                   KFLD                    tblKUT
177600041210     C                   KFLD                    tblCOD
177700000830     C*
177800000613     C                   ENDSR
177900000613     C***
178000990908
178100041206     Otitvg4p   E            riepilogo         2
178200990915     O                                              'Upload via Internet'
178300990915     O                                           +1 'Traduzione TIVIN00R -'
178400060225     O                                           +1 'FIVAB/FIVAT/FIVAX'
178500010201     O                                           +1 'Report testate bolle'
178600990915     O          E            riepilogo   2
178700990915     O                       wrkdata
178800990915     O                       wrkora              +1
178900990915     O                       procname            +1
179000990915     O          E            riepilogo   2
179100990915     O                                              'Cliente..................:'
179200990915     O                       VABCCM        z     +1
179300990915     O          E            riepilogo   2
179400990920     O                                              'Riferimento Strategi.....:'
179500990920     O                       vlrhdl              +1
179600990915     O          E            riepilogo   2
179700990915     O                                              'Giusti...................:'
179800010201     O                       §CTROKVB      2   +  1
179900990915     O          E            riepilogo   2
180000990915     O                                              'Sbagliati e corretti.....:'
180100971022     O                       §CTRMO        2   +  1
180200990915     O          E            riepilogo   2
180300990915     O                                              'Sbagliati e scartati.....:'
180400971022     O                       §CTRNO        2   +  1
180500000613
180600041206     Otitvg4ps  E            testdett          2
180700000613     O                                              'Upload via Internet'
180800000613     O                                           +1 'Traduzione TIVIN00R -'
180900060225     O                                           +1 'FIVAB/FIVAT/FIVAX'
181000010201     O                                           +1 'Report testate bolle'
181100000616     O          E            testdett    3
181200000613     O                                           +2 'N° rec'
181300000613     O                                           +3 'Anteprima contenuto'
181400000616     O          E            rigadett    2
181500000613     O                       rrnum               +2
181600000621     O                       recko               +3
181700000616     O          E            findett     2
181800000613     O                       wrkdata
181900000613     O                       wrkora              +1
182000000613     O                       procname            +1
182100000616     O          E            findett     2
182200000613     O                                              'Cliente..................:'
182300000613     O                       VABCCM        z     +1
182400000616     O          E            findett     2
182500000613     O                                              'Riferimento Strategi.....:'
182600000613     O                       vlrhdl              +1
182700000616     O          E            findett     2
182800000613     O                                              'Giusti...................:'
182900010201     O                       §CTROKVB      2   +  1
183000000616     O          E            findett     2
183100000613     O                                              'Sbagliati e corretti.....:'
183200000613     O                       §CTRMO        2   +  1
183300000616     O          E            findett     2
183400000613     O                                              'Sbagliati e scartati.....:'
183500000613     O                       §CTRNO        2   +  1
183600000512** CMD - COMANDI CL
183700041206OVRPRTF FILE(TITVG4P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
183800041206OVRPRTF FILE(TITVG4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
183900041206DLTOVR FILE(TITVG4P TITVG4PS) LVL(*)
184000000512
184100000512
