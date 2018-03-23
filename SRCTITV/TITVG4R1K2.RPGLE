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
007600060314     D  sT1_LOGO                     28
007700060225     D  sT1_MITT                     25
007800060314     D  sT1_BLK1                      3
007900060225     D  sT1_DEST                     55
008000060225     D*
008100060227     D DS__T_2         DS                  INZ
008200060313     D  sT2_TES1_1                   18
008300060313     D  sT2_TES1_2                   25
008400060314     D  sT2_BLK1                      3
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
009900060314     D  sD1_CONST2                   60    INZ('Codice materiale/Descrizione')
010000060313     D  sD1_CONST3                   12    INZ('Qtà spedita')
010100060314     D  sD1_CONST4                    2    INZ
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
011500060314     D  sD6_MATNUM                   60
011600060313     D  sD6_QTA                      12
011700060314     D  sD6_BLK1                      2
011800060228     D  sD6_QTAORD                   12
011900060314     D  sD6_BLK2                      2
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
014100060314     D  sD9_DATSCAD                  20
014200060314     D  sD9_BLK3                      2
014300060314     D  sD9_QTASPED                  12
014400060227     D*
014500060227     D DS__E_1         DS                  INZ
014600060227     D  sE1_CONST1                  100    INZ('INFORMAZIONI SU RESIDUO DA EVAD-
014700060228     D                                     ERE')
014800060227     D*
014900060227     D DS__E_2         DS                  INZ
015000060227     D  sE2_CONST1                   12    INZ('Nr.Doc. ')
015100060227     D  sE2_CONST2                    7    INZ('Pos.')
015200060227     D  sE2_CONST3                   20    INZ('Materiale')
015300060314     D  sE2_CONST4                   42    INZ('Descrizione')
015400060314     D  sE2_BLK1                      5
015500060227     D  sE2_CONST5                   17    INZ('Qtà Ordinata')
015600060314     D  sE2_BLK2                      1
015700060314     D  sE2_CONST6                   17    INZ('Qtà Residua')
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
019300060314 ccc D wDS_A1          DS                  INZ
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
025500060314     D*
025600060314     D DS_B99          DS                  INZ
025700060314     D  dB6_DETPOSNUM                 6
025800060314     D  dB6_DETMATNUM                18
025900060314     D  dB6_DETMATDES                40
026000060314     D  dB6_DETBARCOD                10
026100060314     D  dB6_DETQTASPE                15
026200060314     D  dB6_DETUNIT                   3
026300060314     D  dB1_QTADIF                   15
026400060314     D  dB1_UNIT                      3
026500060314     D  dB1_QTAORD                   15
026600060314     D  dB2_CLIORDNUM                35
026700060314     D  dB2_CLIORDDAT                10
026800060314     D  dB3_CLIORDNUM                35
026900060314     D  dB3_CLIORDDAT                10
027000060314     D  dB4_DATSCAD                  30
027100060314     D  dB4_1_DATSCAD                30
027200060314     D  dB5_IDOCNUM                  10
027300060225     D*
027400060314     D wDS_B99         DS                  INZ
027500060314     D wdB6_DETPOSNUM                 6
027600060314     D wdB6_DETMATNUM                18
027700060314     D wdB6_DETMATDES                40
027800060314     D wdB6_DETBARCOD                10
027900060314     D wdB6_DETQTASPE                15
028000060314     D wdB6_DETUNIT                   3
028100060314     D wdB1_QTADIF                   15
028200060314     D wdB1_UNIT                      3
028300060314     D wdB1_QTAORD                   15
028400060314     D wdB2_CLIORDNUM                35
028500060314     D wdB2_CLIORDDAT                10
028600060314     D wdB3_CLIORDNUM                35
028700060314     D wdB3_CLIORDDAT                10
028800060314     D wdB4_DATSCAD                  30
028900060314     D wdB4_1_DATSCAD                30
029000060314     D wdB5_IDOCNUM                  10
029100060314     D*
029200060225
029300060225     D*------------------
029400060225     D* DEFINIZIONE SCHIERE X CONFORMAZIONE LAYOUT PACKING LIST
029500060225     D*------------------
029600060314     D skVAR_PK        S                   DIM(200) LIKE(DS_B99) INZ(*blanks)   * schiera Packings
029700060314     D jVAR_PK         S              3  0 INZ(*zeros)                          * suo indice
029800060314     D skVAR_RS        S                   DIM(200) LIKE(DS_A1)  INZ(*blanks)   * schiera Residui
029900060227     D jVAR_RS         S              3  0 INZ(*zeros)                          * suo indice
030000990908
030100010201
030200010201
030300990915     C                   time                    wrkdata
030400990915     C                   time                    wrkora
030500000913     C                   reset                   rrnum
030600990921     C                   reset                   esito
030700990921     C                   reset                   wrkesito
030800000724     C*
030900000724     C* SE OCCORRE SPEDIRE IN FILIALE
031000000724     C                   if        vlrpoi <> *zeros
031100000724     C*
031200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
031300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
031400000724     C     vlrpoi        chain     azorg01l
031500041210     C                   if        %found(azorg01l)
031600000616     C                   movel(p)  CMD(1)        depcmd
031700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
031800000616     C*
031900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
032000011113     C                   Z-ADD     150           LENGH            15 5
032100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
032200000616     C                   PARM                    depcmd
032300000616     C                   PARM                    LENGH
032400000724     C*
032500000724     C                   endif
032600000724     C                   endif
032700000616     C*
032800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
032900000616     C                   movel(p)  CMD(2)        depcmd
033000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
033100000616     C*
033200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
033300011113     C                   Z-ADD     150           LENGH            15 5
033400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
033500000616     C                   PARM                    depcmd
033600000616     C                   PARM                    LENGH
033700000616     C*
033800041206     C                   if        not %open(titvg4ps)
033900041206     C                   open      titvg4ps
034000000616     C                   except    testdett
034100000613     C                   endif
034200000613     C*
034300041210     C                   EXSR      CARTAB                                       CARICA TABELLE
034400040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
034500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
034600000613     C*
034700010202     C* Effettuo la chiamata al CLLE preposto
034800060225     C                   call(e)   'TITVVTXC'
034900010202     C                   parm                    parccm
035000010202     C                   parm                    parmbr
035100010202     C                   parm      '2'           paropz
035200010202     C*
035300041206     C                   if        %open(titvg4ps)
035400000616     C                   except    findett
035500041206     C                   close     titvg4ps
035600000613     C                   endif
035700000616     C*
035800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
035900011113     C                   Z-ADD     150           LENGH            15 5
036000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
036100000616     C                   PARM                    CMD(3)
036200000616     C                   PARM                    LENGH
036300000616     C*
036400010201     C                   seton                                        LR
036500000613
036600000613
036700000613     C*--------------------------------------------------------
036800000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
036900000613     C*--------------------------------------------------------
037000000621     C     STPR          BEGSR
037100000613     C*
037200041206     C                   if        not %open(titvg4p)
037300041206     C                   open      titvg4p
037400990915     C                   endif
037500990915     C*
037600990915     C                   except    riepilogo
037700990915     C*
037800041206     C                   if        %open(titvg4p)
037900041206     C                   close     titvg4p
038000990914     C                   endif
038100990910     C*
038200000613     C                   ENDSR
038300000613     C***
038400041210
038500041210     C*--------------------------------------------------------
038600041210     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
038700041210     C*--------------------------------------------------------
038800041210     C     CARTAB        BEGSR
038900041210     C*
039000041210     C* TABELLA '15' - NAZIONI
039100041210     C                   clear                   skNAZISO
039200041210     C                   clear                   skNAZBAR
039300060227     C                   clear                   skNAZBARDES
039400041210     C                   eval      tblKUT = 1
039500041210     C                   eval      tblCOD = '15'
039600041210     C     KEYtabP       setll     tabel00f
039700041210     C     KEYtabP       reade     tabel00f
039800041210     C                   dow       not %eof(tabel00f)
039900041210     C                   if        tblFLG = *blanks
040000041210     C                   movel(p)  tblUNI        ds15
040100041210     C                   add       1             jNAZ
040200041210     C                   eval      skNAZBAR(jNAZ) = tblKEY
040300060227     C                   eval      skNAZBARDES(jNAZ) = §15DES
040400041210     C                   eval      skNAZISO(jNAZ) = §15COD
040500041210     C                   endif
040600041210     C     KEYtabP       reade     tabel00f
040700041210     C                   enddo
040800041210     C*
040900041210     C                   ENDSR
041000041210     C***
041100041210
041200041210
041300041210
041400910830     C*--------------------------------------------------------
041500060225     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR e FIVAXWWR  *
041600910830     C*--------------------------------------------------------
041700040526     C     RWFILE        BEGSR
041800990910     C*
041900990914     C                   if        not %open(tivin00r)
042000990908     C                   open      tivin00r
042100990914     C                   endif
042200021113     C                   if        not %open(fivabwwr)
042300021113     C                   open      fivabwwr
042400990914     C                   endif
042500021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
042600060225     C                   exsr      prevatvax
042700010201     C*
042800010202     C                   if        chkcall = '0'
042900010202     C*
043000021113     C                   if        not %open(fivatwwr)
043100021113     C                   open      fivatwwr
043200010201     C                   endif
043300060225     C*
043400060225     C                   if        not %open(fivaxwwr)
043500060225     C                   open      fivaxwwr
043600060225     C                   endif
043700990910     C*
043800010201     C                   clear                   §CTROKVB          5 0
043900020305     C                   clear                   §CTROKVT          5 0
044000060225     C                   clear                   §CTROKVX          5 0
044100000801     C                   clear                   §CTRMO            5 0
044200000801     C                   clear                   §CTRNO            5 0
044300040910     C*
044400921023     C                   DO        *HIVAL
044500990913     C*
044600990915     C                   READ      tivin00r                               70
044700040910     C                   if        vindta > *blanks
044800000613     C                   add       1             rrnum
044900000801     C*
045000000801     C                   if        *in70 = *off
045100000801     C                             and
045200000801     C                             (vinflg = *blanks
045300000801     C                              or vinflg = '0'
045400000801     C                              or vinflg = '2')
045500000801     C*
045600000801     C                   clear                   vinmsg
045700000801     C                   eval      vinflg = '1'
045800040910     C*
045900040910     C* Eseguo routine d traduzione
046000060225     C                   exsr      impvbvtvx
046100040802     C*
046200010305     C                   endif
046300000905     C*
046400000905     C                   else
046500000905     C                   eval      vinflg = '1'
046600000905     C                   endif
046700000905     C*
046800000905     C  N70              update    tivin000
046900000905     C*
047000991022     C  N70              ENDdo
047100041210     C*
047200060227     C* Scarico i buffer eventualmente rimasti in sospeso
047300041210     C                   IF        FlgNewBol = '1'
047400041210     C                   WRITE     FIVAB000
047500060313     C                   IF        VATNOT_B <> *blanks
047600060313     C                   EVAL      VATTRC = VATTRC_B
047700060313     C                   EVAL      VATNOT = VATNOT_B
047800041210     C                   WRITE     FIVAT000
047900041210     C                   ENDIF
048000060313     C                   IF        FlgDKC   =  'S'
048100060313     C                   IF        VATNOT_E <> *blanks
048200060313     C                   EVAL      VATTRC = VATTRC_E
048300060313     C                   EVAL      VATNOT = VATNOT_E
048400060313     C                   WRITE     FIVAT000
048500060313     C                   ENDIF
048600060313     C                   ENDIF
048700060314     C* Memorizzo gli ultimi dati rimasti "in canna"
048800060314     C                   ADD       1             jVAR_PK
048900060314     C                   EVAL      skVAR_PK(jVAR_PK) = DS_B99
049000060227     C                   EXSR      WRIVAX
049100041210     C                   ENDIF
049200010202     C*
049300010202     C                   endif
049400990910
049500990910     C* Se non ci sono record con errori ...
049600000710     C                   if        §ctrno = 0
049700990910     C* ... restituisco esito OK.
049800990921     C                   eval      wrkesito = '0'
049900990910     C                   else
050000010201     C                   if        §ctrokvb > 0
050100990921     C                   eval      wrkesito = '1'
050200000710     C                   else
050300000710     C                   eval      wrkesito = '2'
050400990910     C                   endif
050500000710     C                   endif
050600990910     C*
050700990914     C                   if        %open(tivin00r)
050800990908     C                   close     tivin00r
050900990914     C                   endif
051000021113     C                   if        %open(fivabwwr)
051100021113     C                   close     fivabwwr
051200990914     C                   endif
051300021113     C                   if        %open(fivatwwr)
051400021113     C                   close     fivatwwr
051500010201     C                   endif
051600060225     C                   if        %open(fivaxwwr)
051700060225     C                   close     fivaxwwr
051800060225     C                   endif
051900990910     C*
052000010201     C                   if        §ctrokvb > 0
052100000724     C                             and vlrpoi <> *zeros
052200010202     C                   exsr      invio
052300990920     C                   endif
052400990920     C*
052500910830     C                   ENDSR
052600000613     C***
052700990920
052800000801     C*----------------------------------------------------*
052900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053000000801     C*----------------------------------------------------*
053100010201     C     INZVAR        BEGSR
053200000801     C*
053300040802     C                   Z-ADD     *zeros        Num5_0            5 0
053400040802     C                   MOVEL     '0'           FlgCAS            1
053500060313     C                   MOVEL     '0'           FlgDKC            1
053600060313     C                   Z-ADD     *zeros        wQtaDif          30 9
053700060313     C                   Z-ADD     *zeros        wQtaOrd          30 9
053800060313     C                   Z-ADD     *zeros        wQtaEvasa        30 3
053900060227     C*
054000060227     C* Inizializzo tutte le schiere e le ds d wrk
054100060227     C                   CLEAR                   DS_A1
054200060227     C                   CLEAR                   DS_A2
054300060227     C                   CLEAR                   DS_A3
054400060227     C                   CLEAR                   DS_A4
054500060227     C                   CLEAR                   DS_A5
054600060227     C                   CLEAR                   DS_A6
054700060228     C***                CLEAR                   DS_A7
054800060227     C                   CLEAR                   DS_A8
054900060314     C                   CLEAR                   DS_B99
055000060227     C*
055100060314     C                   CLEAR                   skVAR_PK
055200060227     C                   Z-ADD     *zeros        jVAR_PK
055300060228     C                   CLEAR                   skVAR_RS
055400060227     C                   Z-ADD     *zeros        jVAR_RS
055500000801     C*
055600000801     C                   ENDSR
055700000801     C*----------------------------------------------------*
055800040910     C*  IMPOSTAZIONE CAMPI COSTANTI
055900000801     C*----------------------------------------------------*
056000000801     C     DEFCAM        BEGSR
056100000801     C*
056200021113     C                   CLEAR                   FIVAB000
056300040802     C                   CLEAR                   FIVAT000
056400060225     C                   CLEAR                   FIVAX000
056500020619     C* Imposto i valori di default...
056600041210     C                   Z-ADD     0431893       VABCCM
056700041210     C                   Z-ADD     0431893       VATCCM
056800060225     C                   Z-ADD     0431893       VAXCCM
056900041210     C                   Z-ADD     043           VABLNP
057000041210     C                   Z-ADD     043           VATLNP
057100060225     C                   Z-ADD     043           VAXLNP
057200041210     C                   Z-ADD     000           VABCTR
057300060228     C                   MOVEL     '7A'          VABCTM
057400040823     C                   MOVEL     '1'           VABCBO
057500020619     C* ... e poi verifico se sono stati passati come parametri
057600020619     C                   IF        vlrppt > *blanks
057700040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
057800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
057900020619     C                   EXSR      CHKNUM
058000020619     C                   IF        PiInt=*on
058100020619     C                   Z-ADD     PiVal         VABCCM
058200020619     C                   Z-ADD     PiVal         VATCCM
058300060225     C                   Z-ADD     PiVal         VAXCCM
058400020619     C                   ENDIF
058500040506     C                   ENDIF
058600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
058700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
058800020619     C                   EXSR      CHKNUM
058900020619     C                   IF        PiInt=*on
059000020619     C                   Z-ADD     PiVal         VABLNP
059100020619     C                   Z-ADD     PiVal         VATLNP
059200060225     C                   Z-ADD     PiVal         VAXLNP
059300040506     C                   ENDIF
059400020619     C                   ENDIF
059500040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
059600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
059700020619     C                   EXSR      CHKNUM
059800020619     C                   IF        PiInt=*on
059900020619     C                   Z-ADD     PiVal         VABCTR
060000040506     C                   ENDIF
060100020619     C                   ENDIF
060200041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
060300041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
060400041210     C                   ENDIF
060500060313     C                   EVAL      FlgDKC = %subst(vlrppt:16:1)
060600020619     C                   ENDIF
060700000801     C*
060800000801     C                   ENDSR
060900000801     C*----------------------------------------------------*
061000060225     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT e FIVAX)
061100000801     C*----------------------------------------------------*
061200060225     C     IMPVBVTVX     BEGSR
061300041210     C*
061400041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
061500041210     C                   Z-ADD     *zeros        errore            1 0
061600060313     C*
061700060313     C********
061800060313     C* Tipo record 'E1EDL20' (dati testata: riferimenti, peso, volume, colli, etc..)
061900060313     C********
062000060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL20'
062100060314     C*
062200060314     C* Memorizzo i dati rimasti "in canna" dalla bolla precedente
062300060314     C                   ADD       1             jVAR_PK
062400060314     C                   EVAL      skVAR_PK(jVAR_PK) = DS_B99
062500060314     C                   CLEAR                   DS_B99
062600060313     C*
062700060313     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del FIVAB
062800060313     C                   IF        FlgNewBol = '1'
062900060313     C                   WRITE     FIVAB000
063000060313     C                   IF        VATNOT_B <> *blanks
063100060313     C                   EVAL      VATTRC = VATTRC_B
063200060313     C                   EVAL      VATNOT = VATNOT_B
063300060313     C                   WRITE     FIVAT000
063400060313     C                   ENDIF
063500060313     C                   IF        FlgDKC   =  'S'
063600060313     C                   IF        VATNOT_E <> *blanks
063700060313     C                   EVAL      VATTRC = VATTRC_E
063800060313     C                   EVAL      VATNOT = VATNOT_E
063900060313     C                   WRITE     FIVAT000
064000060313     C                   ENDIF
064100060313     C                   ENDIF
064200060313     C                   EXSR      WRIVAX
064300060313     C                   EVAL      FlgNewBol = '0'
064400060313     C                   ENDIF
064500060313     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
064600060313     C                   EXSR      INZVAR
064700060313     C                   EXSR      DEFCAM
064800060313     C                   EVAL      FlgNewBol = '1'
064900060313     C* Valorizzazione => DS_A8
065000060313     C                   EVALR     dA8_NUMDOC   = %trim(%subst(vindta:63+1:10))
065100060313     C                   MOVE(P)   dA8_NUMDOC    wNum10_0         10 0
065200060313     C                   EVALR     dA8_NUMDOC   = %trim(%editc(wNum10_0:'4'))
065300060313     C                   EVAL      dA8_PORTO1   = %trim(%subst(vindta:115+1:3))
065400060313     C                   EVAL      dA8_PORTO2   = %trim(%subst(vindta:118+1:28))
065500060313     C                   EVAL      dA8_ROUTE    = %trim(%subst(vindta:146+1:6))
065600060313     C                   EVALR     dA8_PESOTOT  = %trim(%subst(vindta:154+1:17))
065700060313     C                   EVAL      PiStr=dA8_PESOTOT
065800060313     C                   EXSR      CHKNUM
065900060313     C                   IF        PiNum=*on
066000060313     C                   IF        PiInt=*on
066100060313     C                   Z-ADD     PiVal         wNum30_0         30 0
066200060313     C                   EVALR     dA8_PESOTOT  = %trim(%editc(wNum30_0:'2'))
066300060313     C                   ELSE
066400060313     C                   Z-ADD     PiVal         wNum30_3         30 3
066500060313     C                   EVALR     dA8_PESOTOT  = %trim(%editc(wNum30_3:'2'))
066600060313     C                   ENDIF
066700060313     C                   ENDIF
066800060313     C                   EVALR     dA8_PESONET  = %trim(%subst(vindta:171+1:15))
066900060313     C                   EVAL      PiStr=dA8_PESONET
067000060313     C                   EXSR      CHKNUM
067100060313     C                   IF        PiNum=*on
067200060313     C                   IF        PiInt=*on
067300060313     C                   Z-ADD     PiVal         wNum30_0         30 0
067400060313     C                   EVALR     dA8_PESONET  = %trim(%editc(wNum30_0:'2'))
067500060313     C                   ELSE
067600060313     C                   Z-ADD     PiVal         wNum30_3         30 3
067700060313     C                   EVALR     dA8_PESONET  = %trim(%editc(wNum30_3:'2'))
067800060313     C                   ENDIF
067900060313     C                   ENDIF
068000060313     C                   EVAL      dA8_PESOUNIT = %trim(%subst(vindta:186+1:3))
068100060313     C                   EVALR     dA8_VOLTOT   = %trim(%subst(vindta:189+1:15))
068200060313     C                   EVAL      PiStr=dA8_VOLTOT
068300060313     C                   EXSR      CHKNUM
068400060313     C                   IF        PiNum=*on
068500060313     C                   IF        PiInt=*on
068600060313     C                   Z-ADD     PiVal         wNum30_0         30 0
068700060313     C                   EVALR     dA8_VOLTOT   = %trim(%editc(wNum30_0:'2'))
068800060313     C                   ELSE
068900060313     C                   Z-ADD     PiVal         wNum30_3         30 3
069000060313     C                   EVALR     dA8_VOLTOT   = %trim(%editc(wNum30_3:'2'))
069100060313     C                   ENDIF
069200060313     C                   ENDIF
069300060313     C                   EVAL      dA8_VOLUNIT  = %trim(%subst(vindta:204+1:3))
069400060313     C                   EVALR     dA8_COLLI    = %trim(%subst(vindta:207+1:5))
069500060313     C                   MOVE(P)   dA8_COLLI     wNum5_0           5 0
069600060313     C                   EVALR     dA8_COLLI    = %trim(%editc(wNum5_0:'2'))
069700060313     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
069800060313     C                   MOVEL     datcor        VABAAS
069900060313     C                   MOVEL     datcor        VATAAS
070000060313     C                   MOVEL     datcor        VAXAAS
070100060313     C                   MOVE      datcor        VABMGS
070200060313     C                   MOVE(P)   vlrpoi        VABFGS
070300060313     C                   MOVE(P)   vlrpoi        VATFGS
070400060313     C                   MOVE(P)   vlrpoi        VAXFGS
070500060313     C* ......VABNSP/VATNSP/VAXNSP => Stacco un numeratore da AZNUM
070600060313     C                   CLEAR                   TRUL33DS
070700060313     C                   EVAL      I33OPE = *zeros
070800060313     C                   EVAL      I33CNU = 302
070900060313     C                   EVAL      I33NUM = 1
071000060313     C                   MOVEL     TRUL33DS      KPJBU
071100060313     C                   CALL      'TRUL33R'
071200060313     C                   PARM                    KPJBA
071300060313     C                   MOVEL     KPJBU         TRUL33DS
071400060313     C                   IF        O33ERR = *zeros
071500060313     C                   Z-ADD     O33NRF        VABNSP
071600060313     C                   Z-ADD     O33NRF        VATNSP
071700060313     C                   Z-ADD     O33NRF        VAXNSP
071800060313     C                   ELSE
071900060313     C                   ADD       1             errore
072000060313     C                   EVAL      vinmsg = %trimr(vinmsg)
072100060313     C                             + ' ' + 'VABNSP VATNSP VAXNSP'
072200060313     C                   ENDIF
072300060313     C* ......VABRMN
072400060313     C                   EVAL      PiStr=%trim(%subst(vindta:64:10))
072500060313     C                   EXSR      CHKNUM
072600060313     C                   IF        PiInt=*on
072700060313     C                   Z-ADD     PiVal         VABRMN
072800060313     C                   ELSE
072900060313     C                   Z-ADD     1             VABRMN
073000060313     C                   ADD       1             errore
073100060313     C                   EVAL      vinmsg = %trimr(vinmsg)
073200060313     C                             + ' ' + 'VABRMN'
073300060313     C                   ENDIF
073400060313     C* ......VABRMA
073500060313     C                   EVAL      VABRMA=%trim(%subst(vindta:64:10))
073600060313     C* ......VABPKB
073700060313     C                   EVAL      PiStr=%trim(%subst(vindta:155:17))
073800060313     C                   EXSR      CHKNUM
073900060313     C                   IF        PiNum=*on
074000060313     C                   IF        %trim(%subst(vindta:187:3)) = 'GRM'          * da grammi a kili
074100060313     C                   EVAL      PiVal=PiVal/1000
074200060313     C                   Z-ADD     PiVal         VABPKB
074300060313     C                   ELSE
074400060313     C                   Z-ADD     PiVal         VABPKB                        * altrimenti dft. kg
074500060313     C                   ENDIF
074600060313     C                   ELSE
074700060313     C                   ADD       1             errore
074800060313     C                   EVAL      vinmsg = %trimr(vinmsg)
074900060313     C                             + ' ' + 'VABPKB'
075000060313     C                   ENDIF
075100060313     C* ......VABVLB
075200060313     C                   EVAL      PiStr=%trim(%subst(vindta:190:15))
075300060313     C                   EXSR      CHKNUM
075400060313     C                   IF        PiNum=*on
075500060313     C                   IF        %trim(%subst(vindta:205:3)) = 'DMQ'          * da dec3 a m3
075600060313     C                   EVAL      PiVal=PiVal/1000
075700060313     C                   Z-ADD     PiVal         VABVLB
075800060313     C                   ELSE
075900060313     C                   Z-ADD     PiVal         VABVLB                         * altrimenti dft. m3
076000060313     C                   ENDIF
076100060313     C                   ELSE
076200060313     C                   ADD       1             errore
076300060313     C                   EVAL      vinmsg = %trimr(vinmsg)
076400060313     C                             + ' ' + 'VABVLB'
076500060313     C                   ENDIF
076600060313     C* ......VABNCL
076700060313     C                   EVAL      PiStr=%trim(%subst(vindta:208:5))
076800060313     C                   EXSR      CHKNUM
076900060313     C                   IF        PiInt=*on
077000060313     C                   Z-ADD     PiVal         VABNCL
077100060313     C                   ELSE
077200060313     C                   ADD       1             errore
077300060313     C                   EVAL      vinmsg = %trimr(vinmsg)
077400060313     C                             + ' ' + 'VABNCL'
077500060313     C                   ENDIF
077600060313     C*
077700060313     C                   ENDIF
077800060227     C*
077900060227     C********
078000060227     C* Tipo record 'Z1EDL14'
078100060227     C********
078200060227     C                   IF        %trim(%subst(vindta:1:10)) = 'Z1EDL14'
078300060227     C* Valorizzazione => DS_A1
078400060228     C                   EVAL      dA1_NUMORD   = %trim(%subst(vindta:63+1:10))
078500060228     C                   MOVE(P)   dA1_NUMORD    wNum10_0         10 0
078600060228     C                   EVALR     dA1_NUMORD   = %trim(%editc(wNum10_0:'4'))
078700060227     C                   EVAL      dA1_POS      = %trim(%subst(vindta:73+1:6))
078800060228     C                   EVALR     dA1_MATNUM   = %trim(%subst(vindta:79+1:18))
078900060227     C                   MOVE(P)   dA1_MATNUM    wNum18_0         18 0
079000060228     C                   EVALR     dA1_MATNUM   = %trim(%editc(wNum18_0:'4'))
079100060227     C                   EVAL      dA1_MATDES   = %trim(%subst(vindta:97+1:40))
079200060228     C                   EVALR     dA1_QTAORD   = %trim(%subst(vindta:137+1:15))
079300060227     C                   EVAL      PiStr=dA1_QTAORD
079400060227     C                   EXSR      CHKNUM
079500060228     C                   IF        PiNum=*on
079600060228     C                   IF        PiInt=*on
079700060228     C                   Z-ADD     PiVal         wNum30_0         30 0
079800060228     C                   EVALR     dA1_QTAORD   = %trim(%editc(wNum30_0:'2'))
079900060228     C                   ELSE
080000060228     C                   Z-ADD     PiVal         wNum30_3         30 3
080100060228     C                   EVALR     dA1_QTAORD   = %trim(%editc(wNum30_3:'2'))
080200060228     C                   ENDIF
080300060228     C                   ENDIF
080400060228     C                   EVALR     dA1_QTADIF   = %trim(%subst(vindta:152+1:15))
080500060227     C                   EVAL      PiStr=dA1_QTADIF
080600060227     C                   EXSR      CHKNUM
080700060228     C                   IF        PiNum=*on
080800060228     C                   IF        PiInt=*on
080900060228     C                   Z-ADD     PiVal         wNum30_0         30 0
081000060228     C                   EVALR     dA1_QTADIF   = %trim(%editc(wNum30_0:'2'))
081100060228     C                   ELSE
081200060228     C                   Z-ADD     PiVal         wNum30_3         30 3
081300060228     C                   EVALR     dA1_QTADIF   = %trim(%editc(wNum30_3:'2'))
081400060228     C                   ENDIF
081500060228     C                   ENDIF
081600060227     C                   EVAL      dA1_UNIT     = %trim(%subst(vindta:167+1:3))
081700060227     C*
081800060227     C* Trattandosi d record "ripetuto" memorizzo in specifica schiera
081900060228     C                   ADD       1             jVAR_RS
082000060228     C                   EVAL      skVAR_RS(jVAR_RS) = DS_A1
082100060227     C                   ENDIF
082200060227     C*
082300060227     C********
082400060227     C* Tipo record 'Z1EDL20'
082500060227     C********
082600060227     C                   IF        %trim(%subst(vindta:1:10)) = 'Z1EDL20'
082700060227     C* Valorizzazione => DS_A2
082800060227     C                   EVAL      dA2_CONTAT   = %trim(%subst(vindta:130+1:40))
082900060227     C                   EVAL      dA2_TEL      = %trim(%subst(vindta:170+1:30))
083000060313     C* Tolgo il prefisso internazionale x l'Italia
083100060313     C                   IF        %subst(%trim(dA2_TEL):1:5) = '0039/'
083200060313     C                   EVAL      dA2_TEL      = %subst(%trim(dA2_TEL):6)
083300060313     C                   ENDIF
083400060313     C                   EVAL      dA2_FAX      = %trim(%subst(vindta:200+1:30))
083500060313     C* Tolgo il prefisso internazionale x l'Italia
083600060313     C                   IF        %subst(%trim(dA2_FAX):1:5) = '0039/'
083700060313     C                   EVAL      dA2_FAX      = %subst(%trim(dA2_FAX):6)
083800060313     C                   ENDIF
083900060227     C                   ENDIF
084000060227     C*
084100060227     C********
084200060313     C* Tipo record 'E1ADRM1'
084300060227     C* - soto tipo 'AG' => Mittente
084400060227     C********
084500060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM1' AND
084600060227     C                             %trim(%subst(vindta:64:3)) = 'AG'
084700060227     C* Valorizzazione => DS_A3
084800060228     C                   EVALR     dA3_MITTID   = %trim(%subst(vindta:67+1:17))
084900060227     C                   MOVE(P)   dA3_MITTID    wNum17_0         17 0
085000060228     C                   EVALR     dA3_MITTID   = %trim(%editc(wNum17_0:'4'))
085100060227     C                   EVAL      dA3_MITTRSD  = %trim(%subst(vindta:118+1:40))
085200060227     C                   EVAL      dA3_MITTIND  = %trim(%subst(vindta:438+1:40))
085300060227     C                   EVAL      dA3_MITTCAP  = %trim(%subst(vindta:572+1:10))
085400060227     C                   EVAL      dA3_MITTLOC  = %trim(%subst(vindta:607+1:40))
085500060227     C                   EVAL      dA3_MITTNAZ  = %trim(%subst(vindta:937+1:3))
085600060227     C                   ENDIF
085700060313     C*
085800060313     C********
085900060313     C* Tipo record 'E1ADRM1' (dati instradamento: rag. soc. dest, indirizzo dest, etc...)
086000060313     C* - soto tipo 'WE' => Destinatario Merce
086100060313     C********
086200060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM1' AND
086300060313     C                             %trim(%subst(vindta:64:3)) = 'WE'
086400060313     C* Valorizzazione => DS_A4
086500060313     C                   EVALR     dA4_DESTID   = %trim(%subst(vindta:67+1:17))
086600060313     C                   MOVE(P)   dA4_DESTID    wNum17_0         17 0
086700060313     C                   EVALR     dA4_DESTID   = %trim(%editc(wNum17_0:'4'))
086800060313     C                   EVAL      dA4_DESTRSD  = %trim(%subst(vindta:118+1:17))
086900060313     C                   EVAL      dA4_DESTRD2  = %trim(%subst(vindta:158+1:40))
087000060313     C                   EVAL      dA4_DESTIND  = %trim(%subst(vindta:438+1:40))
087100060313     C                   EVAL      dA4_DESTCAP  = %trim(%subst(vindta:572+1:10))
087200060313     C                   EVAL      dA4_DESTLOC  = %trim(%subst(vindta:607+1:40))
087300060313     C                   EVAL      dA4_DESTNAZ  = %trim(%subst(vindta:937+1:3))
087400060313     C                   EVAL      dA4_DESTPRV  = %trim(%subst(vindta:943+1:3))
087500060313     C*
087600060313     C* ......VABRSD/VABRD2
087700060313     C                   MOVEL     *blanks       wRSD             70
087800060313     C                   EVAL      wRSD = %trim(%subst(vindta:119:40)) + ' ' +
087900060313     C                                    %trim(%subst(vindta:159:40)) + ' ' +
088000060313     C                                    %trim(%subst(vindta:199:40)) + ' ' +
088100060313     C                                    %trim(%subst(vindta:239:40))
088200060313     C                   EVAL      VABRSD=%subst(wRSD:1:35)
088300060313     C                   EVAL      VABRD2=%subst(wRSD:1+35:35)
088400060313     C* ......VABIND
088500060313     C                   EVAL      VABIND=%trim(%subst(vindta:439:40)) + ' ' +
088600060313     C                                    %trim(%subst(vindta:479:40)) + ' ' +
088700060313     C                                    %trim(%subst(vindta:519:40)) + ' ' +
088800060313     C                                    %trim(%subst(vindta:559:40))
088900060313     C* ......VABCAD
089000060313     C                   EVAL      PiStr=%trim(%subst(vindta:573:10))
089100060313     C                   EXSR      CHKNUM
089200060313     C                   IF        PiInt=*on
089300060313     C                   Z-ADD     PiVal         Num5_0
089400060313     C                   MOVEL(P)  Num5_0        VABCAD
089500060313     C                   ELSE
089600060313     C                   ADD       1             errore
089700060313     C                   EVAL      vinmsg = %trimr(vinmsg)
089800060313     C                             + ' ' + 'VABCAD'
089900060313     C                   ENDIF
090000060313     C* ......VABLOD
090100060313     C                   EVAL      VABLOD=%trim(%subst(vindta:608:40)) + ' ' +
090200060313     C                                    %trim(%subst(vindta:648:40))
090300060313     C* ......VATNOT + Tipo record "B"
090400060313     C                   EVAL      VATNOT_B=%trim(%subst(vindta:748:30)) + ' ' +
090500060313     C                                      %trim(%subst(vindta:778:40))
090600060313     C                   EVAL      VATTRC_B = 'B'
090700060313     C* ......VABNZD (conversone da formato *ISO a formato "BARTOLINI"
090800060313     C                   EVAL      VABNZD=%trim(%subst(vindta:938:3))
090900060313     C                   Z-ADD     1             jNAZ
091000060313     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
091100060313     C                   IF        %found
091200060313     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
091300060313     C                   ENDIF
091400060313     C* ......VABPRD
091500060313     C                   EVAL      VABPRD=%trim(%subst(vindta:944:3))
091600060313     C*
091700060313     C                   ENDIF
091800060227     C*
091900060227     C********
092000060227     C* Tipo record 'E1ADRM1'
092100060227     C* - soto tipo 'OSO' => Mittente originale
092200060227     C********
092300060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM1' AND
092400060227     C                             %trim(%subst(vindta:64:3)) = 'OSO'
092500060227     C* Valorizzazione => DS_A5
092600060227     C                   EVAL      dA5_ORIGRSD  = %trim(%subst(vindta:118+1:40))
092700060227     C                   EVAL      dA5_ORIGIND  = %trim(%subst(vindta:438+1:40))
092800060227     C                   EVAL      dA5_ORIGCIV  = %trim(%subst(vindta:558+1:4))
092900060227     C                   EVAL      dA5_ORIGCAP  = %trim(%subst(vindta:572+1:10))
093000060227     C                   EVAL      dA5_ORIGLOC  = %trim(%subst(vindta:607+1:40))
093100060227     C                   EVAL      dA5_ORIGNAZ  = %trim(%subst(vindta:937+1:3))
093200060227     C                   Z-ADD     1             jNAZ
093300060227     C     dA5_ORIGNAZ   LOOKUP    skNAZISO(jNAZ)                         55
093400060227     C                   IF        %found
093500060227     C                   EVAL      dA5_ORIGNAZD = skNAZBARDES(jNAZ)
093600060227     C                   ENDIF
093700060227     C                   ENDIF
093800060227     C*
093900060227     C********
094000060227     C* Tipo record 'E1EDT13'
094100060227     C* - soto tipo '007' => IDoc Delivery
094200060227     C********
094300060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDT13' AND
094400060227     C                             %trim(%subst(vindta:64:3)) = '007'
094500060227     C* Valorizzazione => DS_A6
094600060227     C                   EVAL      dA6_DATSPE   = %trim(%subst(vindta:90+1:8))
094700060227     C                   EVAL      dA6_DATSPE   = %subst(dA6_DATSPE:7:2)+ '.'+
094800060227     C                                            %subst(dA6_DATSPE:5:2)+ '.'+
094900060227     C                                            %subst(dA6_DATSPE:1:4)
095000060313     C                   ENDIF
095100060313     C*
095200060313     C********
095300060314     C* Tipo record 'E1EDL24' Informazioni elementi dettagli articoli/ordine spediz. corrente
095400060313     C********
095500060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL24'
095600060314     C*
095700060314     C* Trattandosi d record "ripetuto" memorizzo in specifica schiera e inizializzo la DS
095800060314     C                   ADD       1             jVAR_PK
095900060314     C                   EVAL      skVAR_PK(jVAR_PK) = DS_B99
096000060314     C                   CLEAR                   DS_B99
096100060314     C* Valorizzazione => DS_B6 (di cui DS_B99)
096200060313     C                   IF        %trim(%subst(vindta:336+1:6)) <> *blanks
096300060313     C                   EVAL      dB6_DETPOSNUM= %trim(%subst(vindta:336+1:6))
096400060313     C                   ELSE
096500060313     C                   EVAL      dB6_DETPOSNUM= %trim(%subst(vindta:63+1:6))
096600060313     C                   ENDIF
096700060313     C                   EVALR     dB6_DETMATNUM= %trim(%subst(vindta:69+1:18))
096800060313     C                   MOVE(P)   dB6_DETMATNUM wNum18_0         18 0
096900060313     C                   EVALR     dB6_DETMATNUM= %trim(%editc(wNum18_0:'4'))
097000060313     C                   EVAL      dB6_DETMATDES= %trim(%subst(vindta:105+1:40))
097100060313     C                   EVAL      dB6_DETBARCOD= %trim(%subst(vindta:207+1:10))
097200060313     C                   EVALR     dB6_DETQTASPE= %trim(%subst(vindta:239+1:15))
097300060313     C                   EVAL      PiStr=dB6_DETQTASPE
097400060313     C                   EXSR      CHKNUM
097500060313     C                   IF        PiNum=*on
097600060313     C                   IF        PiInt=*on
097700060313     C                   Z-ADD     PiVal         wNum30_0         30 0
097800060313     C                   EVALR     dB6_DETQTASPE= %trim(%editc(wNum30_0:'2'))
097900060313     C                   ELSE
098000060313     C                   Z-ADD     PiVal         wNum30_3         30 3
098100060313     C                   EVALR     dB6_DETQTASPE= %trim(%editc(wNum30_3:'2'))
098200060313     C                   ENDIF
098300060313     C                   ENDIF
098400060313     C                   EVAL      dB6_DETUNIT  = %trim(%subst(vindta:254+1:3))
098500060313     C                   ENDIF
098600060313     C*
098700060313     C********
098800060313     C* Tipo record 'Z1EDL24'
098900060313     C********
099000060313     C                   IF        %trim(%subst(vindta:1:10)) = 'Z1EDL24'
099100060314     C* Valorizzazione => DS_B1 (di cui DS_B99)
099200060313     C                   EVALR     dB1_QTADIF   = %trim(%subst(vindta:164+1:15))
099300060313     C                   EVAL      PiStr=dB1_QTADIF
099400060313     C                   EXSR      CHKNUM
099500060313     C                   IF        PiNum=*on
099600060313     C                   IF        PiInt=*on
099700060313     C                   Z-ADD     PiVal         wNum30_0         30 0
099800060313     C                   EVALR     dB1_QTADIF   = %trim(%editc(wNum30_0:'2'))
099900060313     C                   ELSE
100000060313     C                   Z-ADD     PiVal         wNum30_3         30 3
100100060313     C                   EVALR     dB1_QTADIF   = %trim(%editc(wNum30_3:'2'))
100200060313     C                   ENDIF
100300060313     C                   ENDIF
100400060313     C                   Z-ADD     PiVal         wQtaDif
100500060313     C                   EVAL      dB1_UNIT     = %trim(%subst(vindta:179+1:3))
100600060313     C                   EVALR     dB1_QTAORD   = %trim(%subst(vindta:208+1:15))
100700060313     C                   EVAL      PiStr=dB1_QTAORD
100800060313     C                   EXSR      CHKNUM
100900060313     C                   IF        PiNum=*on
101000060313     C                   IF        PiInt=*on
101100060313     C                   Z-ADD     PiVal         wNum30_0         30 0
101200060313     C                   EVALR     dB1_QTAORD   = %trim(%editc(wNum30_0:'2'))
101300060313     C                   ELSE
101400060313     C                   Z-ADD     PiVal         wNum30_3         30 3
101500060313     C                   EVALR     dB1_QTAORD   = %trim(%editc(wNum30_3:'2'))
101600060313     C                   ENDIF
101700060313     C                   Z-ADD     PiVal         wQtaOrd
101800060313     C                   ENDIF
101900060313     C                   ENDIF
102000060313     C*
102100060313     C********
102200060313     C* Tipo record 'E1EDL15'
102300060313     C* - soto tipo '9999999400' => IDoc Caratteristiche
102400060313     C********
102500060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL15' AND
102600060313     C                             %trim(%subst(vindta:64:10)) = '9999999400'
102700060314     C* Valorizzazione => DS_B4 (di cui DS_B99)
102800060313     C                   EVAL      dB4_DATSCAD  = %trim(%subst(vindta:133+1:30))
102900060313     C                   ENDIF
103000060313     C*
103100060313     C********
103200060313     C* Tipo record 'E1EDL15'
103300060313     C* - soto tipo '0000000823' => IDoc Caratteristiche
103400060313     C********
103500060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL15' AND
103600060313     C                             %trim(%subst(vindta:64:10)) = '0000000823'
103700060314     C* Valorizzazione => DS_B4_1 (di cui DS_B99)
103800060313     C                   EVAL      dB4_1_DATSCAD= %trim(%subst(vindta:133+1:30))
103900060313     C                   ENDIF
104000060313     C*
104100060313     C********
104200060313     C* Tipo record 'E1EDL43'
104300060313     C* - soto tipo 'C' => IDoc Riferimenti
104400060313     C********
104500060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL43' AND
104600060313     C                             %trim(%subst(vindta:64:1)) = 'C'
104700060314     C* Valorizzazione => DS_B5 (di cui DS_B99)
104800060313     C***                EVAL      dB5_IDOCNUM  = %trim(%subst(vindta:64+1:35))
104900060313     C                   EVAL      dB5_IDOCNUM  = %trim(%subst(vindta:64+1:10))
105000060313     C                   MOVE(P)   dB5_IDOCNUM   wNum10_0         10 0
105100060313     C                   EVALR     dB5_IDOCNUM  = %trim(%editc(wNum10_0:'4'))
105200060313     C                   ENDIF
105300060313     C*
105400060313     C********
105500060313     C* Tipo record 'E1EDL41'
105600060313     C* - soto tipo '001' => Riferimenti e date ordini - cliente
105700060313     C********
105800060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL41' AND
105900060313     C                             %trim(%subst(vindta:64:3)) = '001'
106000060314     C* Valorizzazione => DS_B2 (di cui DS_B99)
106100060313     C                   EVAL      dB2_CLIORDNUM= %trim(%subst(vindta:66+1:35))
106200060313     C                   EVAL      dB2_CLIORDDAT= %trim(%subst(vindta:101+1:8))
106300060313     C                   EVAL      dB2_CLIORDDAT= %subst(dB2_CLIORDDAT:7:2)+'.'+
106400060313     C                                            %subst(dB2_CLIORDDAT:5:2)+'.'+
106500060313     C                                            %subst(dB2_CLIORDDAT:1:4)
106600060313     C                   ENDIF
106700060313     C*
106800060313     C********
106900060313     C* Tipo record 'E1EDL41'
107000060313     C* - soto tipo '002' => Riferimenti e date ordini - evasione mittente
107100060313     C********
107200060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL41' AND
107300060313     C                             %trim(%subst(vindta:64:3)) = '002'
107400060314     C* Valorizzazione => DS_B3 (di cui DS_B99)
107500060313     C                   EVAL      dB3_CLIORDNUM= %trim(%subst(vindta:66+1:35))
107600060313     C                   EVAL      dB3_CLIORDDAT= %trim(%subst(vindta:101+1:8))
107700060313     C                   EVAL      dB3_CLIORDDAT= %subst(dB3_CLIORDDAT:7:2)+'.'+
107800060313     C                                            %subst(dB3_CLIORDDAT:5:2)+'.'+
107900060313     C                                            %subst(dB3_CLIORDDAT:1:4)
108000060313     C                   ENDIF
108100060227     C*
108200060227     C********
108300060313     C* Tipo record 'E1ADRM4'
108400060227     C* - soto tipo 'SP' => Vettore
108500060227     C********
108600060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM4' AND
108700060227     C                             %trim(%subst(vindta:64:3)) = 'SP'
108800060227     C* Valorizzazione => DS_A7
108900060227     C                   EVAL      dA7_VETTRSD  = %trim(%subst(vindta:118+1:40))
109000060227     C                   EVAL      dA7_VETTIND  = %trim(%subst(vindta:438+1:40))
109100060227     C                   EVAL      dA7_VETTCAP  = %trim(%subst(vindta:572+1:10))
109200060227     C                   EVAL      dA7_VETTLOC  = %trim(%subst(vindta:607+1:40))
109300060227     C                   EVAL      dA7_VETTNAZ  = %trim(%subst(vindta:937+1:3))
109400060227     C                   ENDIF
109500060313     C*
109600060313     C********
109700060313     C* Tipo record 'E1EDL37' Informazioni "CHI SONO" colli
109800060313     C********
109900060313     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL37'
110000060313     C                   EVAL      VATNOT_E = %trim(%subst(vindta:1+1:20))
110100060313     C                   EVAL      VATTRC_E = 'E'
110200060313     C                   ENDIF
110300040910     C*
110400040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
110500040910     C                   IF        FlgCAS <> '0'
110600040929     C                   IF        VABCBO = '1'
110700040910     C                   EVAL      VABCBO = '4'
110800040910     C                   ELSE
110900040929     C                   EVAL      VABCBO = '6'
111000040910     C                   ENDIF
111100040929     C                   ENDIF
111200040910     C*
111300040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
111400040910     C                   EXSR      CHKIMPDIV
111500010202     C*
111600000801     C* Ebbene...
111700000801     C                   ADD       1             §CTRMO
111800010201     C                   IF        errore <> *zeros
111900000801     C                   ADD       1             §CTRNO
112000000801     C                   EVAL      recko = vindta
112100000801     C                   EXCEPT    rigadett
112200000801     C                   EVAL      vinflg = '2'
112300000801     C                   ELSE
112400010201     C                   ADD       1             §CTROKVB
112500000801     C                   ENDIF
112600000801     C*
112700000801     C                   ENDSR
112800010202     C*----------------------------------------------------*
112900060225     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR E FILE FIVAXWWR
113000010202     C*----------------------------------------------------*
113100060225     C     PREVATVAX     BEGSR
113200010202     C*
113300060225     C* Compongo il nome del membro da dare al FIVATWWR e al FIVAXWWR
113400010202     C                   eval      parmbr = vlrhdl
113500010202     C                   movel     'M'           parmbr
113600041210     C                   eval      parccm = vlrksc
113700010202     C                   eval      paropz = '1'
113800010202     C* Effettuo la chiamata al CLLE preposto
113900060225     C                   call(e)   'TITVVTXC'
114000010202     C                   parm                    parccm
114100010202     C                   parm                    parmbr
114200010202     C                   parm                    paropz
114300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
114400010202     C                   if        %error
114500010202     C                   movel     '1'           chkcall
114600010202     C                   else
114700010202     C                   movel     '0'           chkcall
114800010202     C                   endif
114900010202     C*
115000010202     C                   ENDSR
115100000801     C*----------------------------------------------------*
115200000801     C*  CONTROLLO NUMERICITA' CAMPI
115300000801     C*----------------------------------------------------*
115400000801     C     CHKNUM        BEGSR
115500000801     C*
115600000801     C                   call(e)   'ISNUMERIC'
115700000801     C                   PARM                    PiStr            30
115800041210     C                   PARM      '.'           PiDecChr          1
115900000801     C                   PARM      *ZEROS        PiVal            30 9
116000000801     C                   PARM      '0'           PiInt             1
116100000801     C                   PARM      '0'           PiNum             1
116200000801     C                   IF        %error
116300060227     C                   EVAL      PiNum=*off
116400000801     C                   ENDIF
116500000801     C*
116600000801     C                   ENDSR
116700000801     C***
116800000801
116900011113
117000011113     C*----------------------------------------------------*
117100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
117200011113     C*----------------------------------------------------*
117300011113     C     CHKIMPDIV     BEGSR
117400011113     C*
117500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
117600011113     C                   Z-ADD     *zeros        wrkDec            9 9
117700011113     C*
117800011113     C* Come prima cosa effettuo considerazioni sulla divisa
117900011113     C                   IF        vabIAS > *zeros
118000011113     C                   IF        vabVAS <> 'EUR'
118100011113     C                   EVAL      vabVAS =  'ITL'
118200011113     C                   ENDIF
118300011113     C                   ENDIF
118400011113     C*
118500011113     C                   IF        vabCAS > *zeros
118600011113     C                   IF        vabVCA <> 'EUR'
118700011113     C                   EVAL      vabVCA =  'ITL'
118800011113     C                   ENDIF
118900011113     C                   ENDIF
119000011113     C*
119100011113     C                   IF        vabVMD > *zeros
119200020305     C                   IF        vabVAD <> 'EUR'
119300011113     C                   EVAL      vabVAD =  'ITL'
119400011113     C                   ENDIF
119500011113     C                   ENDIF
119600011113     C*
119700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
119800011113     C                   Z-ADD     vabIAS        wrkDec
119900011113     C                   IF        wrkDec > *zeros
120000011113     C                   IF        vabVAS = 'ITL'
120100011113     C                   EVAL      vabIAS = *zeros
120200011113     C                   ENDIF
120300011113     C                   ENDIF
120400011113     C*
120500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
120600011113     C                   Z-ADD     vabCAS        wrkDec
120700011113     C                   IF        wrkDec > *zeros
120800011113     C                   IF        vabVCA = 'ITL'
120900011113     C                   EVAL      vabCAS = *zeros
121000011113     C                   ENDIF
121100011113     C                   ENDIF
121200011113     C*
121300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
121400011113     C                   Z-ADD     vabVMD        wrkDec
121500011113     C                   IF        wrkDec > *zeros
121600011113     C                   IF        vabVAD = 'ITL'
121700011113     C                   EVAL      vabVMD = *zeros
121800011113     C                   ENDIF
121900011113     C                   ENDIF
122000011113     C*
122100011113     C                   ENDSR
122200011113     C***
122300011113
122400060227
122500060227
122600060227     C*----------------------------------------------------*
122700060227     C*  ROUTINE GENERAZIONE BUFFER FORMATI FILE FIVAXWWR
122800060227     C*----------------------------------------------------*
122900060227     C     WRIVAX        BEGSR
123000060313     C*
123100060313     C* Inizializzo deposito per verifica cambio posizioni articoli/ordini
123200060313     C                   MOVEL     *blanks       wCurrPosiz        6
123300060227     C*
123400060227     C* Inizializzo contatore d riga e flag fine dati DDT x bolla corrente
123500060227     C                   Z-ADD     1             VAXPRR
123600060227     C*
123700060227     C* Imposto il tipo modulo x la stampa del logo personalizzato
123800060227     C                   EVAL      VAXTMD = 'BBRA'
123900060227     C*
124000060227     C* Innanzitutto inizializzo il campo dati flat
124100060227     C                   CLEAR                   VAXDTT
124200060228     C*
124300060228     C* Imposto il carattere in "grassetto"
124400060228     C                   EVAL      VAXTRK = 'T1'
124500060314     C*
124600060314     C* Valorizzo subito la ds che contiene anche alcuni dati d testata con uno
124700060314     C* qualunque (tengo l'ultimo memorizzato) dei dettagli articoli/ordine spedizione corrente
124800060314     C                   EVAL      wDS_B99 = skVAR_PK(jVAR_PK)
124900060227     C*
125000060227     C                   RESET                   DS__T_1
125100060313     C                   EVAL      sT1_MITT = %trim(dA3_MITTRSD)
125200060227     C                   EVAL      sT1_DEST = %trim(dA4_DESTRSD)
125300060227     C                   EVAL      VAXDTT   = DS__T_1
125400060228     C                   WRITE     FIVAX000
125500060228     C                   ADD       1             VAXPRR
125600060227     C*
125700060227     C                   RESET                   DS__T_1
125800060313     C                   EVAL      sT1_MITT = %trim(dA3_MITTIND)
125900060227     C                   EVAL      sT1_DEST = %trim(dA4_DESTRD2)
126000060227     C                   EVAL      VAXDTT   = DS__T_1
126100060228     C                   WRITE     FIVAX000
126200060228     C                   ADD       1             VAXPRR
126300060227     C*
126400060227     C                   RESET                   DS__T_1
126500060313     C                   EVAL      sT1_MITT = %trim(dA3_MITTCAP) + ' ' +
126600060313     C                                        %trim(dA3_MITTLOC)
126700060227     C                   EVAL      sT1_DEST = %trim(dA4_DESTIND)
126800060227     C                   EVAL      VAXDTT   = DS__T_1
126900060228     C                   WRITE     FIVAX000
127000060228     C                   ADD       1             VAXPRR
127100060227     C*
127200060227     C                   RESET                   DS__T_1
127300060313     C                   EVAL      sT1_DEST = %trim(dA4_DESTCAP) + ' '  +
127400060227     C                                        %trim(dA4_DESTLOC) + ' ' +
127500060314     C                                        '(' + %trim(dA4_DESTPRV) + ')'
127600060314     C***                                     %trim(dA4_DESTNAZ)
127700060227     C                   EVAL      VAXDTT   = DS__T_1
127800060228     C                   WRITE     FIVAX000
127900060228     C                   ADD       1             VAXPRR
128000060227     C*
128100060228     C* Imposto il carattere in "normale"
128200060228     C                   EVAL      VAXTRK = 'C1'
128300060228     C*
128400060227     C                   EVAL      VAXDTT   = *blanks
128500060228     C                   WRITE     FIVAX000
128600060228     C                   ADD       1             VAXPRR
128700060227     C*
128800060227     C                   RESET                   DS__T_2
128900060227     C                   EVAL      sT2_TES1_1 = 'Nr.Documento:'
129000060227     C                   EVAL      sT2_TES1_2 = %trim(dA8_NUMDOC)
129100060227     C                   EVAL      sT2_TES2_1 = 'Nr.Ordine vendita:'
129200060313     C***                EVAL      sT2_TES2_2 = %trim(dA1_NUMORD)
129300060314     C                   EVAL      sT2_TES2_2 = %trim(wdB5_IDOCNUM)
129400060227     C                   EVAL      VAXDTT   = DS__T_2
129500060228     C                   WRITE     FIVAX000
129600060228     C                   ADD       1             VAXPRR
129700060227     C*
129800060227     C                   RESET                   DS__T_2
129900060227     C                   EVAL      sT2_TES1_1 = 'Data spedizione:'
130000060227     C                   EVAL      sT2_TES1_2 = %trim(dA6_DATSPE)
130100060227     C                   EVAL      sT2_TES2_1 = 'Codice BBMI:'
130200060227     C                   EVAL      sT2_TES2_2 = %trim(dA4_DESTID)
130300060227     C                   EVAL      VAXDTT   = DS__T_2
130400060228     C                   WRITE     FIVAX000
130500060228     C                   ADD       1             VAXPRR
130600060227     C*
130700060227     C                   RESET                   DS__T_2
130800060227     C                   EVAL      sT2_TES1_1 = 'Itinerario:'
130900060227     C                   EVAL      sT2_TES1_2 = %trim(dA8_ROUTE)
131000060227     C                   EVAL      sT2_TES2_1 = 'Committente:'
131100060313     C                   EVAL      sT2_TES2_2 = %trim(dA3_MITTID)
131200060227     C                   EVAL      VAXDTT   = DS__T_2
131300060228     C                   WRITE     FIVAX000
131400060228     C                   ADD       1             VAXPRR
131500060227     C*
131600060227     C                   RESET                   DS__T_2
131700060313     C                   EVAL      sT2_TES1_1 = 'Tipo porto:'
131800060313     C                   EVAL      sT2_TES1_2 = %trim(dA8_PORTO1) + ' '+
131900060313     C                                          %trim(dA8_PORTO2)
132000060313     C                   EVAL      sT2_TES2_1 = 'Nr.Ordine commit.:'
132100060314     C                   EVAL      sT2_TES2_2 = %trim(wdB2_CLIORDNUM)+' del: '+
132200060314     C                                          %trim(wdB2_CLIORDDAT)
132300060227     C                   EVAL      VAXDTT   = DS__T_2
132400060228     C                   WRITE     FIVAX000
132500060228     C                   ADD       1             VAXPRR
132600060227     C*
132700060227     C                   RESET                   DS__T_2
132800060227     C                   EVAL      sT2_TES2_1 = 'Referente:'
132900060313     C                   EVAL      sT2_TES2_2 = %trim(dA2_CONTAT)+' Tel '+
133000060313     C                                          %trim(dA2_TEL)   +' Fax '+
133100060313     C                                          %trim(dA2_FAX)
133200060227     C                   EVAL      VAXDTT   = DS__T_2
133300060228     C                   WRITE     FIVAX000
133400060228     C                   ADD       1             VAXPRR
133500060227     C*
133600060227     C                   EVAL      VAXDTT   = *blanks
133700060228     C                   WRITE     FIVAX000
133800060228     C                   ADD       1             VAXPRR
133900060313     C*
134000060313     C                   RESET                   DS__D_3
134100060313     C***                EVAL      sD3_DESCR  = %trim(sD3_DESCR)   + ' '    +
134200060313     C***                                       %trim(dA7_VETTRSD) + ' - '  +
134300060313     C***                                       %trim(dA7_VETTIND) + ' - '  +
134400060313     C***                                       %trim(dA7_VETTCAP) + ' '    +
134500060313     C***                                       %trim(dA7_VETTLOC)
134600060313     C                   EVAL      sD3_DESCR  = %trim(sD3_DESCR)   + ' '       +
134700110504     C                                          'BRT S.p.A. - Via Monte '      +
134800060313     C                                          'Pastello, 30 - 37057 San '    +
134900060313     C                                          'Giovanni Lupatoto (VT) - IT '
135000060313     C                   EVAL      VAXDTT   = DS__D_3
135100060313     C                   WRITE     FIVAX000
135200060313     C                   ADD       1             VAXPRR
135300060313     C*
135400060313     C                   EVAL      VAXDTT   = *blanks
135500060313     C                   WRITE     FIVAX000
135600060313     C                   ADD       1             VAXPRR
135700060313     C*
135800060313     C* Imposto il carattere in "normale + grassetto"
135900060313     C                   EVAL      VAXTRK = 'T2'
136000060313     C*
136100060227     C                   RESET                   DS__D_1
136200060227     C                   EVAL      VAXDTT   = DS__D_1
136300060228     C                   WRITE     FIVAX000
136400060228     C                   ADD       1             VAXPRR
136500060313     C*
136600060313     C* Imposto il carattere in "normale"
136700060313     C                   EVAL      VAXTRK = 'C1'
136800060228     C*
136900060228     C                   RESET                   DS__R_1
137000060228     C                   EVAL      VAXDTT   = DS__R_1
137100060228     C                   WRITE     FIVAX000
137200060228     C                   ADD       1             VAXPRR
137300060227     C*
137400060227     C                   EVAL      VAXDTT   = *blanks
137500060228     C                   WRITE     FIVAX000
137600060228     C                   ADD       1             VAXPRR
137700060228     C*
137800060228     C* Se presenti informazioni sui dettagli Packing List => gestisco....
137900060228     C                   IF        jVAR_PK > *zeros
138000060314     C*
138100060314     C* Ordino la schiera dei dettagli articoli/ordine spedizione corrente
138200060314     C                   SORTA     skVAR_PK
138300060314     C*
138400060314     C* ...ciclo su tutti i dettagli (<n> righe)
138500060314     C                   Z-ADD     1             jWRK              3 0
138600060314     C                   DOW       jWRK <= %elem(skVAR_PK)
138700060228     C*
138800060228     C* Finchè memorizzato qualcosa...  altrimenti esco dal ciclo
138900060314     C                   IF        skVAR_PK(jWRK) <> *blanks
139000060313     C*
139100060313     C* Valorizzo la ds dei dettagli articoli/ordine
139200060314     C                   EVAL      wDS_B99 = skVAR_PK(jWRK)
139300060313     C*
139400060313     C* Stampo le seguenti 3 righe solo a cambio d posizione articoli/ordine
139500060314     C                   IF        %trim(wCurrPosiz) <> %trim(wdB6_DETPOSNUM)
139600060313     C*
139700060313     C* Salvo la posizione articolo/ordine corrente
139800060313     C                   EVAL      wCurrPosiz = %trim(wdB6_DETPOSNUM)
139900060313     C*
140000060313     C* Imposto il carattere in "normale + grassetto"
140100060314     C*
140200060314     C* ...riga DETT:  Posizione e numero ordine
140300060313     C                   EVAL      VAXTRK = 'T2'
140400060313     C                   RESET                   DS__D_6
140500060313     C                   EVAL      sD6_POSNUM = %trim(wdB6_DETPOSNUM)
140600060313     C                   EVAL      sD6_MATNUM = %trim(wdB6_DETMATNUM)
140700060314     C                   EVALR     sD6_QTAORD = %trim(wdB1_QTAORD) + ' ' +
140800060314     C                                          %trim(wdB1_UNIT)
140900060314     C                   IF        wdB1_QTADIF = *blanks
141000060314     C                   EVAL      wdB1_QTADIF = '0'
141100060313     C                   ENDIF
141200060314     C                   EVALR     sD6_QTARES = %trim(wdB1_QTADIF) + ' ' +
141300060314     C                                          %trim(wdB1_UNIT)
141400060313     C*
141500060313     C                   EVAL      VAXDTT   = DS__D_6
141600060313     C                   WRITE     FIVAX000
141700060313     C                   ADD       1             VAXPRR
141800060313     C*
141900060314     C* ...riga DETT:  Descrizione articolo
142000060313     C                   RESET                   DS__D_7
142100060313     C                   EVAL      sD7_DETMATDES=%trim(wdB6_DETMATDES)
142200060313     C                   EVAL      VAXDTT   = DS__D_7
142300060313     C                   WRITE     FIVAX000
142400060313     C                   ADD       1             VAXPRR
142500060313     C*
142600060313     C* Imposto il carattere in "normale"
142700060313     C                   EVAL      VAXTRK = 'C1'
142800060314     C*
142900060314     C* ...riga DETT:  Numero e data ordine
143000060313     C                   RESET                   DS__D_8
143100060314     C                   EVAL      sD8_CLIORDNUM=%trim(wdB3_CLIORDNUM)
143200060314     C                   EVAL      sD8_CLIORDDAT=%trim(wdB3_CLIORDDAT)
143300060313     C                   EVAL      VAXDTT   = DS__D_8
143400060313     C                   WRITE     FIVAX000
143500060313     C                   ADD       1             VAXPRR
143600060314     C*
143700060314     C                   ENDIF
143800060314     C*
143900060314     C* ...riga DETT:  Numero parte e data scadenza
144000060228     C                   RESET                   DS__D_9
144100060228     C                   EVAL      sD9_DETBARCOD=%trim(wdB6_DETBARCOD)
144200060314     C                   IF        wdB4_1_DATSCAD <> *blanks
144300060314     C                   EVAL      sD9_DATSCAD  =%trim(wdB4_1_DATSCAD)
144400060314     C                   ELSE
144500060314     C                   EVAL      sD9_DATSCAD  =%trim(wdB4_DATSCAD)
144600060314     C                   ENDIF
144700060314     C                   EVALR     sD9_QTASPED  =%trim(wdB6_DETQTASPE) + ' ' +
144800060314     C                                           %trim(wdB6_DETUNIT)
144900060228     C                   EVAL      VAXDTT   = DS__D_9
145000060313     C*
145100060313     C* Se presenti dati => stampo
145200060314     C                   IF        %trim(sD9_DETBARCOD) <> *blanks OR
145300060314     C                             %trim(sD9_DATSCAD)   <> *blanks
145400060228     C*
145500060228     C* Quindi scarico il buffer della riga corrente del DDT bolla corrente
145600060228     C                   WRITE     FIVAX000
145700060228     C*
145800060228     C* Incremento il contatore d riga corrente
145900060228     C                   ADD       1             VAXPRR
146000060313     C                   ENDIF
146100060314     C*
146200060314     C                   ENDIF
146300060314     C*
146400060314     C* Incremento l'indice della schiera corrente
146500060314     C                   ADD       1             jWRK
146600060314     C*
146700060228     C                   ENDDO
146800060228     C                   ENDIF
146900060228     C*
147000060228     C                   EVAL      VAXDTT   = *blanks
147100060228     C                   WRITE     FIVAX000
147200060228     C                   ADD       1             VAXPRR
147300060227     C*
147400060227     C* Se presenti informazioni sul "Residuo da evadere" => gestisco....
147500060227     C                   IF        jVAR_RS > *zeros
147600060228     C* ...titolo  (riga 26)
147700060227     C                   RESET                   DS__E_1
147800060227     C                   EVAL      VAXDTT   = DS__E_1
147900060228     C                   EVAL      VAXTRK = 'T1'                                * vado in grassetto
148000060227     C                   WRITE     FIVAX000
148100060228     C                   ADD       1             VAXPRR
148200060228     C                   EVAL      VAXTRK = 'C1'                                * torno a normale
148300060228     C* ...intestazioni colonne (riga 27)
148400060227     C                   RESET                   DS__E_2
148500060227     C                   EVAL      VAXDTT   = DS__E_2
148600060227     C                   WRITE     FIVAX000
148700060228     C                   ADD       1             VAXPRR
148800060228     C* ...riga separazione "alta" (riga 28)
148900060228     C                   RESET                   DS__R_1
149000060228     C                   EVAL      VAXDTT   = DS__R_1
149100060227     C                   WRITE     FIVAX000
149200060228     C                   ADD       1             VAXPRR
149300060314     C* ...ciclo su dettaglio articoli/ordini da evadere (<n> righe)
149400060228     C                   Z-ADD     *zeros        jWRK              3 0
149500060228     C                   DO        *hival
149600060228     C*
149700060228     C* Incremento l'indice della schiera corrente
149800060228     C                   ADD       1             jWRK
149900060228     C*
150000060228     C* Finchè memorizzato qualcosa...  altrimenti esco dal ciclo
150100060228     C                   IF        skVAR_RS(jWRK) <> *blanks
150200060228     C                   EVAL      wDS_A1 = skVAR_RS(jWRK)
150300060228     C                   RESET                   DS__E_3
150400060228     C                   EVAL      sE3_NUMORD = %trim(wdA1_NUMORD)
150500060228     C                   EVAL      sE3_POS    = %trim(wdA1_POS)
150600060228     C                   EVAL      sE3_MATNUM = %trim(wdA1_MATNUM)
150700060228     C                   EVAL      sE3_MATDES = %trim(wdA1_MATDES)
150800060314     C                   EVALR     sE3_QTAORD = %trim(wdA1_QTAORD) + ' ' +
150900060228     C                                          %trim(wdA1_UNIT)
151000060314     C                   EVALR     sE3_QTADIF = %trim(wdA1_QTADIF) + ' ' +
151100060228     C                                          %trim(wdA1_UNIT)
151200060228     C                   EVAL      VAXDTT   = DS__E_3
151300060228     C*
151400060228     C* Quindi scarico il buffer della riga corrente del DDT bolla corrente
151500060228     C                   WRITE     FIVAX000
151600060228     C*
151700060228     C* Incremento il contatore d riga corrente
151800060228     C                   ADD       1             VAXPRR
151900060228     C*
152000060228     C                   ELSE
152100060228     C*
152200060228     C* Al primo elemento d schiera nn valorizzato => esco dal ciclo...
152300060228     C                   LEAVE
152400060228     C                   ENDIF
152500060227     C*
152600060227     C                   ENDDO
152700060228     C                   ENDIF
152800060313     C*
152900060228     C                   RESET                   DS__R_1
153000060228     C                   EVAL      VAXDTT   = DS__R_1
153100060228     C                   WRITE     FIVAX000
153200060228     C                   ADD       1             VAXPRR
153300060313     C*
153400060313     C* Imposto il carattere in "normale + grassetto"
153500060313     C                   EVAL      VAXTRK = 'T2'
153600060313     C*
153700060313     C                   RESET                   DS__P_1
153800060228     C                   EVAL      sP1_DICITUR1='Peso totale:'
153900060228     C                   EVAL      sP1_VALORE1 =%trim(dA8_PESOTOT) + ' ' +
154000060228     C                                          %trim(dA8_PESOUNIT)
154100060313     C                   EVAL      sP1_DICITUR2='Peso netto:'
154200060313     C                   EVAL      sP1_VALORE2 =%trim(dA8_PESONET) + ' ' +
154300060313     C                                          %trim(dA8_PESOUNIT)
154400060313     C                   EVAL      sP1_DICITUR3='Volume totale:'
154500060313     C                   EVAL      sP1_VALORE3 =%trim(dA8_VOLTOT) + ' ' +
154600060313     C                                          %trim(dA8_VOLUNIT)
154700060313     C                   EVAL      sP1_DICITUR4='Numero colli:'
154800060313     C                   EVAL      sP1_VALORE4 =%trim(dA8_COLLI)
154900060228     C                   EVAL      VAXDTT   = DS__P_1
155000060228     C                   WRITE     FIVAX000
155100060228     C                   ADD       1             VAXPRR
155200060313     C*
155300060313     C* Imposto il carattere in "normale"
155400060313     C                   EVAL      VAXTRK = 'C1'
155500060228     C*
155600060228     C                   RESET                   DS__R_1
155700060228     C                   EVAL      VAXDTT   = DS__R_1
155800060228     C                   WRITE     FIVAX000
155900060228     C                   ADD       1             VAXPRR
156000060227     C*
156100060227     C                   ENDSR
156200011113
156300000801
156400000801
156500990920      /TITLE Invio dei dati al punto operativo.
156600010202     C     invio         BEGSR
156700060225     C*
156800060225     C* 1° invio FIVAX
156900060225     C                   reset                   dscmz
157000060225     C                   move      vlrpoi        cmzdst
157100060225     C                   eval      cmzfld = 'FIVAXWWR'
157200060225     C                   eval      cmzmbd = vlrhdl
157300060225     C                   eval      %subst(cmzmbd:1:1) = 'M'
157400060225     C***                if        prmfir = *blanks
157500060225     C                   eval      cmzfla = 'FIVAX00F'
157600060225     C                   eval      cmzmba = 'FIVAX00F'
157700060225     C***                else
157800060225     C***                eval      cmzfla = prmfir
157900060225     C***                eval      cmzmba = prmfir
158000060225     C***                endif
158100060225     C                   eval      cmznrr = *zeros
158200060227     C                   move      §ctrokvx      cmznrr
158300060225     C                   eval      cmzlba = vlrfl1
158400060225     C                   call(e)   'TIS711C'
158500060225     C                   parm                    dscmz
158600060225     C                   parm      *blanks       esito
158700060225     C                   if        %error
158800060225     C                             or cmzerr = '1'
158900060225     C                             or esito  = '1'
159000060225     C                   eval      wrkesito = '3'
159100060225     C                   else
159200990920     C*
159300060225     C* 2° invio FIVAT
159400010201     C                   reset                   dscmz
159500010201     C                   move      vlrpoi        cmzdst
159600021113     C                   eval      cmzfld = 'FIVATWWR'
159700010201     C                   eval      cmzmbd = vlrhdl
159800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
159900021009     C***                if        prmfir = *blanks
160000021113     C                   eval      cmzfla = 'FIVAT00F'
160100021113     C                   eval      cmzmba = 'FIVAT00F'
160200021009     C***                else
160300021009     C***                eval      cmzfla = prmfir
160400021009     C***                eval      cmzmba = prmfir
160500021009     C***                endif
160600010201     C                   eval      cmznrr = *zeros
160700020305     C                   move      §ctrokvt      cmznrr
160800021018     C                   eval      cmzlba = vlrfl1
160900010201     C                   call(e)   'TIS711C'
161000010201     C                   parm                    dscmz
161100010201     C                   parm      *blanks       esito
161200010205     C                   if        %error
161300010205     C                             or cmzerr = '1'
161400010205     C                             or esito  = '1'
161500010205     C                   eval      wrkesito = '3'
161600010205     C                   else
161700010201     C*
161800060225     C* 3° invio FIVAB
161900010201     C                   reset                   dscmz
162000010201     C                   move      vlrpoi        cmzdst
162100010201     C                   eval      cmzfld = vlrfou
162200010201     C                   eval      cmzmbd = vlrhdl
162300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
162400021009     C***                if        prmfir = *blanks
162500021113     C                   eval      cmzfla = 'FIVAB00F'
162600021113     C                   eval      cmzmba = 'FIVAB00F'
162700021009     C***                else
162800021009     C***                eval      cmzfla = prmfir
162900021009     C***                eval      cmzmba = prmfir
163000021009     C***                endif
163100010201     C                   eval      cmznrr = *zeros
163200010201     C                   move      §ctrokvb      cmznrr
163300021018     C                   eval      cmzlba = vlrfl1
163400010201     C                   call(e)   'TIS711C'
163500010201     C                   parm                    dscmz
163600010201     C                   parm      *blanks       esito
163700010201     C                   if        %error
163800010201     C                             or cmzerr = '1'
163900010201     C                             or esito  = '1'
164000010201     C                   eval      wrkesito = '3'
164100010201     C                   endif
164200010205     C                   endif
164300060225     C                   endif
164400990920     C*
164500000613     C                   ENDSR
164600000613     C***
164700990910
164800000613     C     *inzsr        BEGSR
164900990910     C*
165000990910     C     *entry        plist
165100990920     C                   parm                    tivlrds
165200990921     C                   parm      wrkesito      esito
165300000724     C                   parm                    prmlit
165400000710     C                   parm                    prmfir
165500000613     C*
165600000830     C* CALCOLA LA DATA CORRENTE
165700000830     C                   time                    wn14             14 0
165800000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
165900000830     C                   z-add     wn8           g08dat
166000000830     C                   z-add     *zeros        g08inv
166100000830     C                   movel     '0'           g08err
166200000830     C                   call      'XSRDA8'
166300000830     C                   parm                    wlbda8
166400000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
166500041210     C*
166600041210     C* Chiave su TABEL00F - parziale
166700041210     C     KEYtabP       KLIST
166800041210     C                   KFLD                    tblKUT
166900041210     C                   KFLD                    tblCOD
167000000830     C*
167100000613     C                   ENDSR
167200000613     C***
167300990908
167400041206     Otitvg4p   E            riepilogo         2
167500990915     O                                              'Upload via Internet'
167600990915     O                                           +1 'Traduzione TIVIN00R -'
167700060225     O                                           +1 'FIVAB/FIVAT/FIVAX'
167800010201     O                                           +1 'Report testate bolle'
167900990915     O          E            riepilogo   2
168000990915     O                       wrkdata
168100990915     O                       wrkora              +1
168200990915     O                       procname            +1
168300990915     O          E            riepilogo   2
168400990915     O                                              'Cliente..................:'
168500990915     O                       VABCCM        z     +1
168600990915     O          E            riepilogo   2
168700990920     O                                              'Riferimento Strategi.....:'
168800990920     O                       vlrhdl              +1
168900990915     O          E            riepilogo   2
169000990915     O                                              'Giusti...................:'
169100010201     O                       §CTROKVB      2   +  1
169200990915     O          E            riepilogo   2
169300990915     O                                              'Sbagliati e corretti.....:'
169400971022     O                       §CTRMO        2   +  1
169500990915     O          E            riepilogo   2
169600990915     O                                              'Sbagliati e scartati.....:'
169700971022     O                       §CTRNO        2   +  1
169800000613
169900041206     Otitvg4ps  E            testdett          2
170000000613     O                                              'Upload via Internet'
170100000613     O                                           +1 'Traduzione TIVIN00R -'
170200060225     O                                           +1 'FIVAB/FIVAT/FIVAX'
170300010201     O                                           +1 'Report testate bolle'
170400000616     O          E            testdett    3
170500000613     O                                           +2 'N° rec'
170600000613     O                                           +3 'Anteprima contenuto'
170700000616     O          E            rigadett    2
170800000613     O                       rrnum               +2
170900000621     O                       recko               +3
171000000616     O          E            findett     2
171100000613     O                       wrkdata
171200000613     O                       wrkora              +1
171300000613     O                       procname            +1
171400000616     O          E            findett     2
171500000613     O                                              'Cliente..................:'
171600000613     O                       VABCCM        z     +1
171700000616     O          E            findett     2
171800000613     O                                              'Riferimento Strategi.....:'
171900000613     O                       vlrhdl              +1
172000000616     O          E            findett     2
172100000613     O                                              'Giusti...................:'
172200010201     O                       §CTROKVB      2   +  1
172300000616     O          E            findett     2
172400000613     O                                              'Sbagliati e corretti.....:'
172500000613     O                       §CTRMO        2   +  1
172600000616     O          E            findett     2
172700000613     O                                              'Sbagliati e scartati.....:'
172800000613     O                       §CTRNO        2   +  1
172900000512** CMD - COMANDI CL
173000041206OVRPRTF FILE(TITVG4P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
173100041206OVRPRTF FILE(TITVG4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
173200041206DLTOVR FILE(TITVG4P TITVG4PS) LVL(*)
173300000512
173400000512
