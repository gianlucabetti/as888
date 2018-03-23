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
012400060228     D  sD4_BLK2                      6
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
013800060227     D*
013900060227     D DS__E_1         DS                  INZ
014000060227     D  sE1_CONST1                  100    INZ('INFORMAZIONI SU RESIDUO DA EVAD-
014100060228     D                                     ERE')
014200060227     D*
014300060227     D DS__E_2         DS                  INZ
014400060227     D  sE2_CONST1                   12    INZ('Nr.Doc. ')
014500060227     D  sE2_CONST2                    7    INZ('Pos.')
014600060227     D  sE2_CONST3                   20    INZ('Materiale')
014700060227     D  sE2_CONST4                   42    INZ('Descrizione')
014800060227     D  sE2_CONST5                   17    INZ('Qtà Ordinata')
014900060227     D  sE2_CONST6                   17    INZ('Qtà Residua')
015000060227     D*
015100060227     D DS__E_3         DS                  INZ
015200060227     D  sE3_NUMORD                   12
015300060227     D  sE3_POS                       7
015400060227     D  sE3_MATNUM                   20
015500060227     D  sE3_MATDES                   42
015600060227     D  sE3_QTAORD                   17
015700060227     D  sE3_QTADIF                   17
015800060228     D*
015900060228     D DS__P_1         DS                  INZ
016000060228     D  sP1_DICITUR1                 15
016100060228     D  sP1_VALORE1                  25
016200060228     D  sP1_BLK1                      5
016300060228     D  sP1_DICITUR2                 40
016400060228     D  sP1_VALORE2                  12
016500060227     D*
016600060225
016700060225     D*------------------
016800060225     D* DEFINIZIONE DS D WRK X RIDEFINIZIONE TIPI RECORD FILE CLIENTE
016900060225     D*------------------
017000060225     D DS_A1           DS                  INZ
017100060225     D  dA1_NUMORD                   10
017200060225     D  dA1_POS                       6
017300060225     D  dA1_MATNUM                   18
017400060225     D  dA1_MATDES                   40
017500060225     D  dA1_QTAORD                   15
017600060225     D  dA1_QTADIF                   15
017700060225     D  dA1_UNIT                      3
017800060228     D*
017900060228     D wDS_A1          DS                  INZ
018000060228     D  wdA1_NUMORD                  10
018100060228     D  wdA1_POS                      6
018200060228     D  wdA1_MATNUM                  18
018300060228     D  wdA1_MATDES                  40
018400060228     D  wdA1_QTAORD                  15
018500060228     D  wdA1_QTADIF                  15
018600060228     D  wdA1_UNIT                     3
018700060225     D*
018800060225     D DS_A2           DS                  INZ
018900060225     D  dA2_CONTAT                   40
019000060225     D  dA2_TEL                      30
019100060225     D  dA2_FAX                      30
019200060225     D*
019300060225     D DS_A3           DS                  INZ
019400060225     D  dA3_MITTID                   17
019500060225     D  dA3_MITTRSD                  40
019600060225     D  dA3_MITTIND                  40
019700060225     D  dA3_MITTCAP                  10
019800060225     D  dA3_MITTLOC                  40
019900060225     D  dA3_MITTNAZ                   3
020000060225     D*
020100060225     D DS_A4           DS                  INZ
020200060227     D  dA4_DESTID                   17
020300060225     D  dA4_DESTRSD                  40
020400060225     D  dA4_DESTRD2                  40
020500060225     D  dA4_DESTIND                  40
020600060225     D  dA4_DESTCAP                  10
020700060225     D  dA4_DESTLOC                  40
020800060225     D  dA4_DESTNAZ                   3
020900060225     D  dA4_DESTPRV                   3
021000060225     D*
021100060225     D DS_A5           DS                  INZ
021200060225     D  dA5_ORIGRSD                  40
021300060225     D  dA5_ORIGIND                  40
021400060225     D  dA5_ORIGCIV                   4
021500060225     D  dA5_ORIGCAP                  10
021600060225     D  dA5_ORIGLOC                  40
021700060227     D  dA5_ORIGNAZ                   3
021800060228     D  dA5_ORIGNAZD                 25
021900060225     D*
022000060225     D DS_A6           DS                  INZ
022100060228     D  dA6_DATSPE                   10
022200060225     D*
022300060225     D DS_A7           DS                  INZ
022400060227     D  dA7_VETTRSD                  40
022500060227     D  dA7_VETTIND                  40
022600060227     D  dA7_VETTCAP                  10
022700060227     D  dA7_VETTLOC                  40
022800060227     D  dA7_VETTNAZ                   3
022900060225     D*
023000060225     D DS_A8           DS                  INZ
023100060225     D  dA8_NUMDOC                   10
023200060225     D  dA8_PORTO1                    3
023300060225     D  dA8_PORTO2                   28
023400060225     D  dA8_ROUTE                     6
023500060225     D  dA8_PESOTOT                  17
023600060225     D  dA8_PESONET                  15
023700060225     D  dA8_PESOUNIT                  3
023800060225     D  dA8_VOLTOT                   15
023900060225     D  dA8_VOLUNIT                   3
024000060225     D  dA8_COLLI                     5
024100060225     D*
024200060225     D DS_B1           DS                  INZ
024300060225     D  dB1_QTADIF                   15
024400060225     D  dB1_UNIT                      3
024500060225     D  dB1_QTAORD                   15
024600060225     D*
024700060225     D DS_B2           DS                  INZ
024800060225     D  dB2_CLIORDNUM                35
024900060228     D  dB2_CLIORDDAT                10
025000060225     D*
025100060225     D DS_B3           DS                  INZ
025200060225     D  dB3_CLIORDNUM                35
025300060228     D  dB3_CLIORDDAT                10
025400060225     D*
025500060225     D DS_B4           DS                  INZ
025600060225     D  dB4_DATSCAD                  30
025700060225     D*
025800060225     D DS_B4_1         DS                  INZ
025900060225     D  dB4_1_DATSCAD                30
026000060225     D*
026100060225     D DS_B5           DS                  INZ
026200060228     D***  dB5_IDOCNUM                  35
026300060228     D  dB5_IDOCNUM                  10
026400060225     D*
026500060225     D DS_B6           DS                  INZ
026600060225     D  dB6_DETPOSNUM                 6
026700060225     D  dB6_DETMATNUM                18
026800060225     D  dB6_DETMATDES                40
026900060225     D  dB6_DETBARCOD                10
027000060225     D  dB6_DETQTASPE                15
027100060225     D  dB6_DETUNIT                   3
027200060225     D*
027300060225
027400060225     D*------------------
027500060225     D* DEFINIZIONE SCHIERE X CONFORMAZIONE LAYOUT PACKING LIST
027600060225     D*------------------
027700060228 xxx D skVAR_PK        S                   DIM(100) LIKE(vaxDTT) INZ(*blanks)   * schiera Packings
027800060228 xxx D jVAR_PK         S              3  0 INZ(*zeros)                          * suo indice
027900060228     D skVAR_RS        S                   DIM(100) LIKE(vaxDTT) INZ(*blanks)   * schiera Residui
028000060227     D jVAR_RS         S              3  0 INZ(*zeros)                          * suo indice
028100990908
028200010201
028300010201
028400990915     C                   time                    wrkdata
028500990915     C                   time                    wrkora
028600000913     C                   reset                   rrnum
028700990921     C                   reset                   esito
028800990921     C                   reset                   wrkesito
028900000724     C*
029000000724     C* SE OCCORRE SPEDIRE IN FILIALE
029100000724     C                   if        vlrpoi <> *zeros
029200000724     C*
029300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
029400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
029500000724     C     vlrpoi        chain     azorg01l
029600041210     C                   if        %found(azorg01l)
029700000616     C                   movel(p)  CMD(1)        depcmd
029800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
029900000616     C*
030000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
030100011113     C                   Z-ADD     150           LENGH            15 5
030200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
030300000616     C                   PARM                    depcmd
030400000616     C                   PARM                    LENGH
030500000724     C*
030600000724     C                   endif
030700000724     C                   endif
030800000616     C*
030900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
031000000616     C                   movel(p)  CMD(2)        depcmd
031100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
031200000616     C*
031300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
031400011113     C                   Z-ADD     150           LENGH            15 5
031500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
031600000616     C                   PARM                    depcmd
031700000616     C                   PARM                    LENGH
031800000616     C*
031900041206     C                   if        not %open(titvg4ps)
032000041206     C                   open      titvg4ps
032100000616     C                   except    testdett
032200000613     C                   endif
032300000613     C*
032400041210     C                   EXSR      CARTAB                                       CARICA TABELLE
032500040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
032600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
032700000613     C*
032800010202     C* Effettuo la chiamata al CLLE preposto
032900060225     C                   call(e)   'TITVVTXC'
033000010202     C                   parm                    parccm
033100010202     C                   parm                    parmbr
033200010202     C                   parm      '2'           paropz
033300010202     C*
033400041206     C                   if        %open(titvg4ps)
033500000616     C                   except    findett
033600041206     C                   close     titvg4ps
033700000613     C                   endif
033800000616     C*
033900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
034000011113     C                   Z-ADD     150           LENGH            15 5
034100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
034200000616     C                   PARM                    CMD(3)
034300000616     C                   PARM                    LENGH
034400000616     C*
034500010201     C                   seton                                        LR
034600000613
034700000613
034800000613     C*--------------------------------------------------------
034900000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
035000000613     C*--------------------------------------------------------
035100000621     C     STPR          BEGSR
035200000613     C*
035300041206     C                   if        not %open(titvg4p)
035400041206     C                   open      titvg4p
035500990915     C                   endif
035600990915     C*
035700990915     C                   except    riepilogo
035800990915     C*
035900041206     C                   if        %open(titvg4p)
036000041206     C                   close     titvg4p
036100990914     C                   endif
036200990910     C*
036300000613     C                   ENDSR
036400000613     C***
036500041210
036600041210     C*--------------------------------------------------------
036700041210     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
036800041210     C*--------------------------------------------------------
036900041210     C     CARTAB        BEGSR
037000041210     C*
037100041210     C* TABELLA '15' - NAZIONI
037200041210     C                   clear                   skNAZISO
037300041210     C                   clear                   skNAZBAR
037400060227     C                   clear                   skNAZBARDES
037500041210     C                   eval      tblKUT = 1
037600041210     C                   eval      tblCOD = '15'
037700041210     C     KEYtabP       setll     tabel00f
037800041210     C     KEYtabP       reade     tabel00f
037900041210     C                   dow       not %eof(tabel00f)
038000041210     C                   if        tblFLG = *blanks
038100041210     C                   movel(p)  tblUNI        ds15
038200041210     C                   add       1             jNAZ
038300041210     C                   eval      skNAZBAR(jNAZ) = tblKEY
038400060227     C                   eval      skNAZBARDES(jNAZ) = §15DES
038500041210     C                   eval      skNAZISO(jNAZ) = §15COD
038600041210     C                   endif
038700041210     C     KEYtabP       reade     tabel00f
038800041210     C                   enddo
038900041210     C*
039000041210     C                   ENDSR
039100041210     C***
039200041210
039300041210
039400041210
039500910830     C*--------------------------------------------------------
039600060225     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR e FIVAXWWR  *
039700910830     C*--------------------------------------------------------
039800040526     C     RWFILE        BEGSR
039900990910     C*
040000990914     C                   if        not %open(tivin00r)
040100990908     C                   open      tivin00r
040200990914     C                   endif
040300021113     C                   if        not %open(fivabwwr)
040400021113     C                   open      fivabwwr
040500990914     C                   endif
040600021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
040700060225     C                   exsr      prevatvax
040800010201     C*
040900010202     C                   if        chkcall = '0'
041000010202     C*
041100021113     C                   if        not %open(fivatwwr)
041200021113     C                   open      fivatwwr
041300010201     C                   endif
041400060225     C*
041500060225     C                   if        not %open(fivaxwwr)
041600060225     C                   open      fivaxwwr
041700060225     C                   endif
041800990910     C*
041900010201     C                   clear                   §CTROKVB          5 0
042000020305     C                   clear                   §CTROKVT          5 0
042100060225     C                   clear                   §CTROKVX          5 0
042200000801     C                   clear                   §CTRMO            5 0
042300000801     C                   clear                   §CTRNO            5 0
042400040910     C*
042500921023     C                   DO        *HIVAL
042600990913     C*
042700990915     C                   READ      tivin00r                               70
042800040910     C                   if        vindta > *blanks
042900000613     C                   add       1             rrnum
043000000801     C*
043100000801     C                   if        *in70 = *off
043200000801     C                             and
043300000801     C                             (vinflg = *blanks
043400000801     C                              or vinflg = '0'
043500000801     C                              or vinflg = '2')
043600000801     C*
043700000801     C                   clear                   vinmsg
043800000801     C                   eval      vinflg = '1'
043900040910     C*
044000040910     C* Eseguo routine d traduzione
044100060225     C                   exsr      impvbvtvx
044200040802     C*
044300010305     C                   endif
044400000905     C*
044500000905     C                   else
044600000905     C                   eval      vinflg = '1'
044700000905     C                   endif
044800000905     C*
044900000905     C  N70              update    tivin000
045000000905     C*
045100991022     C  N70              ENDdo
045200041210     C*
045300060227     C* Scarico i buffer eventualmente rimasti in sospeso
045400041210     C                   IF        FlgNewBol = '1'
045500041210     C                   WRITE     FIVAB000
045600041210     C                   IF        VATNOT <> *blanks
045700041210     C                   WRITE     FIVAT000
045800041210     C                   ENDIF
045900060227     C                   EXSR      WRIVAX
046000041210     C                   ENDIF
046100010202     C*
046200010202     C                   endif
046300990910
046400990910     C* Se non ci sono record con errori ...
046500000710     C                   if        §ctrno = 0
046600990910     C* ... restituisco esito OK.
046700990921     C                   eval      wrkesito = '0'
046800990910     C                   else
046900010201     C                   if        §ctrokvb > 0
047000990921     C                   eval      wrkesito = '1'
047100000710     C                   else
047200000710     C                   eval      wrkesito = '2'
047300990910     C                   endif
047400000710     C                   endif
047500990910     C*
047600990914     C                   if        %open(tivin00r)
047700990908     C                   close     tivin00r
047800990914     C                   endif
047900021113     C                   if        %open(fivabwwr)
048000021113     C                   close     fivabwwr
048100990914     C                   endif
048200021113     C                   if        %open(fivatwwr)
048300021113     C                   close     fivatwwr
048400010201     C                   endif
048500060225     C                   if        %open(fivaxwwr)
048600060225     C                   close     fivaxwwr
048700060225     C                   endif
048800990910     C*
048900010201     C                   if        §ctrokvb > 0
049000000724     C                             and vlrpoi <> *zeros
049100010202     C                   exsr      invio
049200990920     C                   endif
049300990920     C*
049400910830     C                   ENDSR
049500000613     C***
049600990920
049700000801     C*----------------------------------------------------*
049800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049900000801     C*----------------------------------------------------*
050000010201     C     INZVAR        BEGSR
050100000801     C*
050200040802     C                   Z-ADD     *zeros        Num5_0            5 0
050300040802     C                   MOVEL     '0'           FlgCAS            1
050400060227     C*
050500060227     C* Inizializzo tutte le schiere e le ds d wrk
050600060227     C                   CLEAR                   DS_A1
050700060227     C                   CLEAR                   DS_A2
050800060227     C                   CLEAR                   DS_A3
050900060227     C                   CLEAR                   DS_A4
051000060227     C                   CLEAR                   DS_A5
051100060227     C                   CLEAR                   DS_A6
051200060228     C***                CLEAR                   DS_A7
051300060227     C                   CLEAR                   DS_A8
051400060227     C                   CLEAR                   DS_B1
051500060227     C                   CLEAR                   DS_B2
051600060227     C                   CLEAR                   DS_B3
051700060227     C                   CLEAR                   DS_B4
051800060227     C                   CLEAR                   DS_B4_1
051900060227     C                   CLEAR                   DS_B5
052000060227     C                   CLEAR                   DS_B6
052100060227     C*
052200060227     C                   CLEAR                   skVAR_PK
052300060227     C                   Z-ADD     *zeros        jVAR_PK
052400060228     C                   CLEAR                   skVAR_RS
052500060227     C                   Z-ADD     *zeros        jVAR_RS
052600000801     C*
052700000801     C                   ENDSR
052800000801     C*----------------------------------------------------*
052900040910     C*  IMPOSTAZIONE CAMPI COSTANTI
053000000801     C*----------------------------------------------------*
053100000801     C     DEFCAM        BEGSR
053200000801     C*
053300021113     C                   CLEAR                   FIVAB000
053400040802     C                   CLEAR                   FIVAT000
053500060225     C                   CLEAR                   FIVAX000
053600020619     C* Imposto i valori di default...
053700041210     C                   Z-ADD     0431893       VABCCM
053800041210     C                   Z-ADD     0431893       VATCCM
053900060225     C                   Z-ADD     0431893       VAXCCM
054000041210     C                   Z-ADD     043           VABLNP
054100041210     C                   Z-ADD     043           VATLNP
054200060225     C                   Z-ADD     043           VAXLNP
054300041210     C                   Z-ADD     000           VABCTR
054400060228     C                   MOVEL     '7A'          VABCTM
054500040823     C                   MOVEL     '1'           VABCBO
054600020619     C* ... e poi verifico se sono stati passati come parametri
054700020619     C                   IF        vlrppt > *blanks
054800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
054900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
055000020619     C                   EXSR      CHKNUM
055100020619     C                   IF        PiInt=*on
055200020619     C                   Z-ADD     PiVal         VABCCM
055300020619     C                   Z-ADD     PiVal         VATCCM
055400060225     C                   Z-ADD     PiVal         VAXCCM
055500020619     C                   ENDIF
055600040506     C                   ENDIF
055700040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
055800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
055900020619     C                   EXSR      CHKNUM
056000020619     C                   IF        PiInt=*on
056100020619     C                   Z-ADD     PiVal         VABLNP
056200020619     C                   Z-ADD     PiVal         VATLNP
056300060225     C                   Z-ADD     PiVal         VAXLNP
056400040506     C                   ENDIF
056500020619     C                   ENDIF
056600040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
056700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
056800020619     C                   EXSR      CHKNUM
056900020619     C                   IF        PiInt=*on
057000020619     C                   Z-ADD     PiVal         VABCTR
057100040506     C                   ENDIF
057200020619     C                   ENDIF
057300041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
057400041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
057500041210     C                   ENDIF
057600020619     C                   ENDIF
057700000801     C*
057800000801     C                   ENDSR
057900000801     C*----------------------------------------------------*
058000060225     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT e FIVAX)
058100000801     C*----------------------------------------------------*
058200060225     C     IMPVBVTVX     BEGSR
058300041210     C*
058400041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
058500041210     C                   Z-ADD     *zeros        errore            1 0
058600060227     C*
058700060227     C********
058800060227     C* Tipo record 'Z1EDL14'
058900060227     C********
059000060227     C                   IF        %trim(%subst(vindta:1:10)) = 'Z1EDL14'
059100060227     C* Valorizzazione => DS_A1
059200060228     C                   EVAL      dA1_NUMORD   = %trim(%subst(vindta:63+1:10))
059300060228     C                   MOVE(P)   dA1_NUMORD    wNum10_0         10 0
059400060228     C                   EVALR     dA1_NUMORD   = %trim(%editc(wNum10_0:'4'))
059500060227     C                   EVAL      dA1_POS      = %trim(%subst(vindta:73+1:6))
059600060228     C                   EVALR     dA1_MATNUM   = %trim(%subst(vindta:79+1:18))
059700060227     C                   MOVE(P)   dA1_MATNUM    wNum18_0         18 0
059800060228     C                   EVALR     dA1_MATNUM   = %trim(%editc(wNum18_0:'4'))
059900060227     C                   EVAL      dA1_MATDES   = %trim(%subst(vindta:97+1:40))
060000060228     C                   EVALR     dA1_QTAORD   = %trim(%subst(vindta:137+1:15))
060100060227     C                   EVAL      PiStr=dA1_QTAORD
060200060227     C                   EXSR      CHKNUM
060300060228     C                   IF        PiNum=*on
060400060228     C                   IF        PiInt=*on
060500060228     C                   Z-ADD     PiVal         wNum30_0         30 0
060600060228     C                   EVALR     dA1_QTAORD   = %trim(%editc(wNum30_0:'2'))
060700060228     C                   ELSE
060800060228     C                   Z-ADD     PiVal         wNum30_3         30 3
060900060228     C                   EVALR     dA1_QTAORD   = %trim(%editc(wNum30_3:'2'))
061000060228     C                   ENDIF
061100060228     C                   ENDIF
061200060228     C                   EVALR     dA1_QTADIF   = %trim(%subst(vindta:152+1:15))
061300060227     C                   EVAL      PiStr=dA1_QTADIF
061400060227     C                   EXSR      CHKNUM
061500060228     C                   IF        PiNum=*on
061600060228     C                   IF        PiInt=*on
061700060228     C                   Z-ADD     PiVal         wNum30_0         30 0
061800060228     C                   EVALR     dA1_QTADIF   = %trim(%editc(wNum30_0:'2'))
061900060228     C                   ELSE
062000060228     C                   Z-ADD     PiVal         wNum30_3         30 3
062100060228     C                   EVALR     dA1_QTADIF   = %trim(%editc(wNum30_3:'2'))
062200060228     C                   ENDIF
062300060228     C                   ENDIF
062400060227     C                   EVAL      dA1_UNIT     = %trim(%subst(vindta:167+1:3))
062500060227     C*
062600060227     C* Trattandosi d record "ripetuto" memorizzo in specifica schiera
062700060228     C                   ADD       1             jVAR_RS
062800060228     C                   EVAL      skVAR_RS(jVAR_RS) = DS_A1
062900060227     C                   ENDIF
063000060227     C*
063100060227     C********
063200060227     C* Tipo record 'Z1EDL20'
063300060227     C********
063400060227     C                   IF        %trim(%subst(vindta:1:10)) = 'Z1EDL20'
063500060227     C* Valorizzazione => DS_A2
063600060227     C                   EVAL      dA2_CONTAT   = %trim(%subst(vindta:130+1:40))
063700060227     C                   EVAL      dA2_TEL      = %trim(%subst(vindta:170+1:30))
063800060227     C                   EVAL      dA2_FAX      = %trim(%subst(vindta:200+1:30))
063900060227     C                   ENDIF
064000060227     C*
064100060227     C********
064200060227     C* Tipo record 'E1ADRM1'
064300060227     C* - soto tipo 'AG' => Mittente
064400060227     C********
064500060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM1' AND
064600060227     C                             %trim(%subst(vindta:64:3)) = 'AG'
064700060227     C* Valorizzazione => DS_A3
064800060228     C                   EVALR     dA3_MITTID   = %trim(%subst(vindta:67+1:17))
064900060227     C                   MOVE(P)   dA3_MITTID    wNum17_0         17 0
065000060228     C                   EVALR     dA3_MITTID   = %trim(%editc(wNum17_0:'4'))
065100060227     C                   EVAL      dA3_MITTRSD  = %trim(%subst(vindta:118+1:40))
065200060227     C                   EVAL      dA3_MITTIND  = %trim(%subst(vindta:438+1:40))
065300060227     C                   EVAL      dA3_MITTCAP  = %trim(%subst(vindta:572+1:10))
065400060227     C                   EVAL      dA3_MITTLOC  = %trim(%subst(vindta:607+1:40))
065500060227     C                   EVAL      dA3_MITTNAZ  = %trim(%subst(vindta:937+1:3))
065600060227     C                   ENDIF
065700060227     C*
065800060227     C********
065900060227     C* Tipo record 'E1ADRM1'
066000060227     C* - soto tipo 'OSO' => Mittente originale
066100060227     C********
066200060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM1' AND
066300060227     C                             %trim(%subst(vindta:64:3)) = 'OSO'
066400060227     C* Valorizzazione => DS_A5
066500060227     C                   EVAL      dA5_ORIGRSD  = %trim(%subst(vindta:118+1:40))
066600060227     C                   EVAL      dA5_ORIGIND  = %trim(%subst(vindta:438+1:40))
066700060227     C                   EVAL      dA5_ORIGCIV  = %trim(%subst(vindta:558+1:4))
066800060227     C                   EVAL      dA5_ORIGCAP  = %trim(%subst(vindta:572+1:10))
066900060227     C                   EVAL      dA5_ORIGLOC  = %trim(%subst(vindta:607+1:40))
067000060227     C                   EVAL      dA5_ORIGNAZ  = %trim(%subst(vindta:937+1:3))
067100060227     C                   Z-ADD     1             jNAZ
067200060227     C     dA5_ORIGNAZ   LOOKUP    skNAZISO(jNAZ)                         55
067300060227     C                   IF        %found
067400060227     C                   EVAL      dA5_ORIGNAZD = skNAZBARDES(jNAZ)
067500060227     C                   ENDIF
067600060227     C                   ENDIF
067700060227     C*
067800060227     C********
067900060227     C* Tipo record 'E1EDT13'
068000060227     C* - soto tipo '007' => IDoc Delivery
068100060227     C********
068200060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDT13' AND
068300060227     C                             %trim(%subst(vindta:64:3)) = '007'
068400060227     C* Valorizzazione => DS_A6
068500060227     C                   EVAL      dA6_DATSPE   = %trim(%subst(vindta:90+1:8))
068600060227     C                   EVAL      dA6_DATSPE   = %subst(dA6_DATSPE:7:2)+ '.'+
068700060227     C                                            %subst(dA6_DATSPE:5:2)+ '.'+
068800060227     C                                            %subst(dA6_DATSPE:1:4)
068900060227     C                   ENDIF
069000060227     C*
069100060227     C********
069200060227     C* Tipo record 'E1ADRM4'
069300060227     C* - soto tipo 'SP' => Vettore
069400060227     C********
069500060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM4' AND
069600060227     C                             %trim(%subst(vindta:64:3)) = 'SP'
069700060227     C* Valorizzazione => DS_A7
069800060227     C                   EVAL      dA7_VETTRSD  = %trim(%subst(vindta:118+1:40))
069900060227     C                   EVAL      dA7_VETTIND  = %trim(%subst(vindta:438+1:40))
070000060227     C                   EVAL      dA7_VETTCAP  = %trim(%subst(vindta:572+1:10))
070100060227     C                   EVAL      dA7_VETTLOC  = %trim(%subst(vindta:607+1:40))
070200060227     C                   EVAL      dA7_VETTNAZ  = %trim(%subst(vindta:937+1:3))
070300060227     C                   ENDIF
070400060227     C*
070500060227     C********
070600060227     C* Tipo record 'Z1EDL24'
070700060227     C********
070800060227     C                   IF        %trim(%subst(vindta:1:10)) = 'Z1EDL24'
070900060227     C* Valorizzazione => DS_B1
071000060228     C                   EVALR     dB1_QTADIF   = %trim(%subst(vindta:164+1:15))
071100060227     C                   EVAL      PiStr=dB1_QTADIF
071200060227     C                   EXSR      CHKNUM
071300060228     C                   IF        PiNum=*on
071400060228     C                   IF        PiInt=*on
071500060228     C                   Z-ADD     PiVal         wNum30_0         30 0
071600060228     C                   EVALR     dB1_QTADIF   = %trim(%editc(wNum30_0:'2'))
071700060228     C                   ELSE
071800060228     C                   Z-ADD     PiVal         wNum30_3         30 3
071900060228     C                   EVALR     dB1_QTADIF   = %trim(%editc(wNum30_3:'2'))
072000060228     C                   ENDIF
072100060228     C                   ENDIF
072200060227     C                   EVAL      dB1_UNIT     = %trim(%subst(vindta:179+1:3))
072300060228     C                   EVALR     dB1_QTAORD   = %trim(%subst(vindta:208+1:15))
072400060227     C                   EVAL      PiStr=dB1_QTAORD
072500060227     C                   EXSR      CHKNUM
072600060228     C                   IF        PiNum=*on
072700060228     C                   IF        PiInt=*on
072800060228     C                   Z-ADD     PiVal         wNum30_0         30 0
072900060228     C                   EVALR     dB1_QTAORD   = %trim(%editc(wNum30_0:'2'))
073000060228     C                   ELSE
073100060228     C                   Z-ADD     PiVal         wNum30_3         30 3
073200060228     C                   EVALR     dB1_QTAORD   = %trim(%editc(wNum30_3:'2'))
073300060228     C                   ENDIF
073400060228     C                   ENDIF
073500060227     C                   ENDIF
073600060227     C*
073700060227     C********
073800060227     C* Tipo record 'E1EDL41'
073900060227     C* - soto tipo '001' => Riferimenti e date ordini - cliente
074000060227     C********
074100060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL41' AND
074200060227     C                             %trim(%subst(vindta:64:3)) = '001'
074300060227     C* Valorizzazione => DS_B2
074400060227     C                   EVAL      dB2_CLIORDNUM= %trim(%subst(vindta:66+1:35))
074500060227     C                   EVAL      dB2_CLIORDDAT= %trim(%subst(vindta:101+1:8))
074600060227     C                   EVAL      dB2_CLIORDDAT= %subst(dB2_CLIORDDAT:7:2)+'.'+
074700060227     C                                            %subst(dB2_CLIORDDAT:5:2)+'.'+
074800060227     C                                            %subst(dB2_CLIORDDAT:1:4)
074900060227     C                   ENDIF
075000060227     C*
075100060227     C********
075200060227     C* Tipo record 'E1EDL41'
075300060227     C* - soto tipo '002' => Riferimenti e date ordini - evasione mittente
075400060227     C********
075500060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL41' AND
075600060227     C                             %trim(%subst(vindta:64:3)) = '002'
075700060227     C* Valorizzazione => DS_B3
075800060227     C                   EVAL      dB3_CLIORDNUM= %trim(%subst(vindta:66+1:35))
075900060227     C                   EVAL      dB3_CLIORDDAT= %trim(%subst(vindta:101+1:8))
076000060227     C                   EVAL      dB3_CLIORDDAT= %subst(dB3_CLIORDDAT:7:2)+'.'+
076100060227     C                                            %subst(dB3_CLIORDDAT:5:2)+'.'+
076200060227     C                                            %subst(dB3_CLIORDDAT:1:4)
076300060227     C                   ENDIF
076400060227     C*
076500060227     C********
076600060227     C* Tipo record 'E1EDL15'
076700060227     C* - soto tipo '9999999400' => IDoc Caratteristiche
076800060227     C********
076900060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL15' AND
077000060227     C                             %trim(%subst(vindta:64:10)) = '9999999400'
077100060227     C* Valorizzazione => DS_B4
077200060227     C                   EVAL      dB4_DATSCAD  = %trim(%subst(vindta:133+1:30))
077300060227     C                   ENDIF
077400060227     C*
077500060227     C********
077600060227     C* Tipo record 'E1EDL15'
077700060227     C* - soto tipo '0000000823' => IDoc Caratteristiche
077800060227     C********
077900060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL15' AND
078000060227     C                             %trim(%subst(vindta:64:10)) = '0000000823'
078100060227     C* Valorizzazione => DS_B4_1
078200060227     C                   EVAL      dB4_1_DATSCAD= %trim(%subst(vindta:133+1:30))
078300060227     C                   ENDIF
078400060227     C*
078500060227     C********
078600060227     C* Tipo record 'E1EDL43'
078700060227     C* - soto tipo 'C' => IDoc Riferimenti
078800060227     C********
078900060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL43' AND
079000060227     C                             %trim(%subst(vindta:64:1)) = 'C'
079100060227     C* Valorizzazione => DS_B5
079200060228     C***                EVAL      dB5_IDOCNUM  = %trim(%subst(vindta:64+1:35))
079300060228     C                   EVAL      dB5_IDOCNUM  = %trim(%subst(vindta:64+1:10))
079400060228     C                   MOVE(P)   dB5_IDOCNUM   wNum10_0         10 0
079500060228     C                   EVALR     dB5_IDOCNUM  = %trim(%editc(wNum10_0:'4'))
079600060227     C                   ENDIF
079700060227     C*
079800060227     C********
079900060227     C* Tipo record 'E1EDL24' Informazioni elementi testata spedizione
080000060227     C********
080100060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL24'
080200060227     C* Valorizzazione => DS_B6
080300060227     C                   IF        %trim(%subst(vindta:336+1:6)) <> *blanks
080400060227     C                   EVAL      dB6_DETPOSNUM= %trim(%subst(vindta:336+1:6))
080500060227     C                   ELSE
080600060227     C                   EVAL      dB6_DETPOSNUM= %trim(%subst(vindta:63+1:6))
080700060227     C                   ENDIF
080800060228     C                   EVALR     dB6_DETMATNUM= %trim(%subst(vindta:69+1:18))
080900060227     C                   MOVE(P)   dB6_DETMATNUM wNum18_0         18 0
081000060228     C                   EVALR     dB6_DETMATNUM= %trim(%editc(wNum18_0:'4'))
081100060227     C                   EVAL      dB6_DETMATDES= %trim(%subst(vindta:105+1:40))
081200060227     C                   EVAL      dB6_DETBARCOD= %trim(%subst(vindta:207+1:10))
081300060228     C                   EVALR     dB6_DETQTASPE= %trim(%subst(vindta:239+1:15))
081400060227     C                   EVAL      PiStr=dB6_DETQTASPE
081500060227     C                   EXSR      CHKNUM
081600060228     C                   IF        PiNum=*on
081700060228     C                   IF        PiInt=*on
081800060228     C                   Z-ADD     PiVal         wNum30_0         30 0
081900060228     C                   EVALR     dB6_DETQTASPE= %trim(%editc(wNum30_0:'2'))
082000060228     C                   ELSE
082100060228     C                   Z-ADD     PiVal         wNum30_3         30 3
082200060228     C                   EVALR     dB6_DETQTASPE= %trim(%editc(wNum30_3:'2'))
082300060228     C                   ENDIF
082400060228     C                   ENDIF
082500060227     C                   EVAL      dB6_DETUNIT  = %trim(%subst(vindta:254+1:3))
082600060227     C*
082700060227     C* Trattandosi d record "ripetuto" memorizzo in specifica schiera
082800060228     C                   ADD       1             jVAR_PK
082900060228     C                   EVAL      skVAR_PK(jVAR_PK) = DS_B6
083000060227     C                   ENDIF
083100060227     C*
083200041210     C********
083300050204     C* Tipo record 'E1EDL20' (dati testata: riferimenti, peso, volume, colli, etc..)
083400041210     C********
083500060227     C                   IF        %trim(%subst(vindta:1:10)) = 'E1EDL20'
083600060227     C* Valorizzazione => DS_A8
083700060228     C                   EVALR     dA8_NUMDOC   = %trim(%subst(vindta:63+1:10))
083800060227     C                   MOVE(P)   dA8_NUMDOC    wNum10_0         10 0
083900060228     C                   EVALR     dA8_NUMDOC   = %trim(%editc(wNum10_0:'4'))
084000060227     C                   EVAL      dA8_PORTO1   = %trim(%subst(vindta:115+1:3))
084100060227     C                   EVAL      dA8_PORTO2   = %trim(%subst(vindta:118+1:28))
084200060227     C                   EVAL      dA8_ROUTE    = %trim(%subst(vindta:146+1:6))
084300060228     C                   EVALR     dA8_PESOTOT  = %trim(%subst(vindta:154+1:17))
084400060227     C                   EVAL      PiStr=dA8_PESOTOT
084500060227     C                   EXSR      CHKNUM
084600060228     C                   IF        PiNum=*on
084700060228     C                   IF        PiInt=*on
084800060228     C                   Z-ADD     PiVal         wNum30_0         30 0
084900060228     C                   EVALR     dA8_PESOTOT  = %trim(%editc(wNum30_0:'2'))
085000060228     C                   ELSE
085100060228     C                   Z-ADD     PiVal         wNum30_3         30 3
085200060228     C                   EVALR     dA8_PESOTOT  = %trim(%editc(wNum30_3:'2'))
085300060228     C                   ENDIF
085400060228     C                   ENDIF
085500060228     C                   EVALR     dA8_PESONET  = %trim(%subst(vindta:171+1:15))
085600060227     C                   EVAL      PiStr=dA8_PESONET
085700060227     C                   EXSR      CHKNUM
085800060228     C                   IF        PiNum=*on
085900060228     C                   IF        PiInt=*on
086000060228     C                   Z-ADD     PiVal         wNum30_0         30 0
086100060228     C                   EVALR     dA8_PESONET  = %trim(%editc(wNum30_0:'2'))
086200060228     C                   ELSE
086300060228     C                   Z-ADD     PiVal         wNum30_3         30 3
086400060228     C                   EVALR     dA8_PESONET  = %trim(%editc(wNum30_3:'2'))
086500060228     C                   ENDIF
086600060228     C                   ENDIF
086700060227     C                   EVAL      dA8_PESOUNIT = %trim(%subst(vindta:186+1:3))
086800060228     C                   EVALR     dA8_VOLTOT   = %trim(%subst(vindta:189+1:15))
086900060227     C                   EVAL      PiStr=dA8_VOLTOT
087000060227     C                   EXSR      CHKNUM
087100060228     C                   IF        PiNum=*on
087200060228     C                   IF        PiInt=*on
087300060228     C                   Z-ADD     PiVal         wNum30_0         30 0
087400060228     C                   EVALR     dA8_VOLTOT   = %trim(%editc(wNum30_0:'2'))
087500060228     C                   ELSE
087600060228     C                   Z-ADD     PiVal         wNum30_3         30 3
087700060228     C                   EVALR     dA8_VOLTOT   = %trim(%editc(wNum30_3:'2'))
087800060228     C                   ENDIF
087900060228     C                   ENDIF
088000060227     C                   EVAL      dA8_VOLUNIT  = %trim(%subst(vindta:204+1:3))
088100060228     C                   EVALR     dA8_COLLI    = %trim(%subst(vindta:207+1:5))
088200060227     C                   MOVE(P)   dA8_COLLI     wNum5_0           5 0
088300060228     C                   EVALR     dA8_COLLI    = %trim(%editc(wNum5_0:'2'))
088400060227     C*
088500041210     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del FIVAB
088600041210     C                   IF        FlgNewBol = '1'
088700041210     C                   WRITE     FIVAB000
088800041210     C                   IF        VATNOT <> *blanks
088900041210     C                   WRITE     FIVAT000
089000041210     C                   ENDIF
089100060227     C                   EXSR      WRIVAX
089200060227     C                   EVAL      FlgNewBol = '0'
089300041210     C                   ENDIF
089400041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
089500040910     C                   EXSR      INZVAR
089600040910     C                   EXSR      DEFCAM
089700041210     C                   EVAL      FlgNewBol = '1'
089800040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
089900040928     C                   MOVEL     datcor        VABAAS
090000040928     C                   MOVEL     datcor        VATAAS
090100060225     C                   MOVEL     datcor        VAXAAS
090200040928     C                   MOVE      datcor        VABMGS
090300040928     C                   MOVE(P)   vlrpoi        VABFGS
090400040928     C                   MOVE(P)   vlrpoi        VATFGS
090500060225     C                   MOVE(P)   vlrpoi        VAXFGS
090600060225     C* ......VABNSP/VATNSP/VAXNSP => Stacco un numeratore da AZNUM
090700041210     C                   CLEAR                   TRUL33DS
090800041210     C                   EVAL      I33OPE = *zeros
090900041210     C                   EVAL      I33CNU = 302
091000041210     C                   EVAL      I33NUM = 1
091100041210     C                   MOVEL     TRUL33DS      KPJBU
091200041210     C                   CALL      'TRUL33R'
091300041210     C                   PARM                    KPJBA
091400041210     C                   MOVEL     KPJBU         TRUL33DS
091500041210     C                   IF        O33ERR = *zeros
091600041210     C                   Z-ADD     O33NRF        VABNSP
091700041210     C                   Z-ADD     O33NRF        VATNSP
091800060225     C                   Z-ADD     O33NRF        VAXNSP
091900041210     C                   ELSE
092000041210     C                   ADD       1             errore
092100041210     C                   EVAL      vinmsg = %trimr(vinmsg)
092200060225     C                             + ' ' + 'VABNSP VATNSP VAXNSP'
092300041210     C                   ENDIF
092400041210     C* ......VABRMN
092500041210     C                   EVAL      PiStr=%trim(%subst(vindta:64:10))
092600041210     C                   EXSR      CHKNUM
092700041210     C                   IF        PiInt=*on
092800041210     C                   Z-ADD     PiVal         VABRMN
092900041210     C                   ELSE
093000041210     C                   Z-ADD     1             VABRMN
093100041210     C                   ADD       1             errore
093200041210     C                   EVAL      vinmsg = %trimr(vinmsg)
093300041210     C                             + ' ' + 'VABRMN'
093400041210     C                   ENDIF
093500041210     C* ......VABRMA
093600041210     C                   EVAL      VABRMA=%trim(%subst(vindta:64:10))
093700041210     C* ......VABPKB
093800041210     C                   EVAL      PiStr=%trim(%subst(vindta:155:17))
093900041210     C                   EXSR      CHKNUM
094000041210     C                   IF        PiNum=*on
094100041210     C                   IF        %trim(%subst(vindta:187:3)) = 'GRM'          * da grammi a kili
094200041210     C                   EVAL      PiVal=PiVal/1000
094300050209     C                   Z-ADD     PiVal         VABPKB
094400041210     C                   ELSE
094500050209     C                   Z-ADD     PiVal         VABPKB                        * altrimenti dft. kg
094600041210     C                   ENDIF
094700041210     C                   ELSE
094800041210     C                   ADD       1             errore
094900041210     C                   EVAL      vinmsg = %trimr(vinmsg)
095000041210     C                             + ' ' + 'VABPKB'
095100041210     C                   ENDIF
095200041210     C* ......VABVLB
095300041210     C                   EVAL      PiStr=%trim(%subst(vindta:190:15))
095400041210     C                   EXSR      CHKNUM
095500041210     C                   IF        PiNum=*on
095600041210     C                   IF        %trim(%subst(vindta:205:3)) = 'DMQ'          * da dec3 a m3
095700041210     C                   EVAL      PiVal=PiVal/1000
095800050209     C                   Z-ADD     PiVal         VABVLB
095900041210     C                   ELSE
096000050209     C                   Z-ADD     PiVal         VABVLB                         * altrimenti dft. m3
096100041210     C                   ENDIF
096200041210     C                   ELSE
096300041210     C                   ADD       1             errore
096400041210     C                   EVAL      vinmsg = %trimr(vinmsg)
096500041210     C                             + ' ' + 'VABVLB'
096600041210     C                   ENDIF
096700041210     C* ......VABNCL
096800041210     C                   EVAL      PiStr=%trim(%subst(vindta:208:5))
096900041210     C                   EXSR      CHKNUM
097000041210     C                   IF        PiInt=*on
097100041210     C                   Z-ADD     PiVal         VABNCL
097200041210     C                   ELSE
097300041210     C                   ADD       1             errore
097400041210     C                   EVAL      vinmsg = %trimr(vinmsg)
097500041210     C                             + ' ' + 'VABNCL'
097600041210     C                   ENDIF
097700041210     C*
097800041210     C                   ENDIF
097900041210     C*
098000041210     C********
098100050204     C* Tipo record 'E1ADRM1' (dati instradamento: rag. soc. dest, indirizzo dest, etc...)
098200041210     C* - soto tipo 'WE' => Destinatario Merce
098300041210     C********
098400050204     C                   IF        %trim(%subst(vindta:1:10)) = 'E1ADRM1' AND
098500041210     C                             %trim(%subst(vindta:64:3)) = 'WE'
098600060227     C* Valorizzazione => DS_A4
098700060228     C                   EVALR     dA4_DESTID   = %trim(%subst(vindta:67+1:17))
098800060227     C                   MOVE(P)   dA4_DESTID    wNum17_0         17 0
098900060228     C                   EVALR     dA4_DESTID   = %trim(%editc(wNum17_0:'4'))
099000060227     C                   EVAL      dA4_DESTRSD  = %trim(%subst(vindta:118+1:17))
099100060227     C                   EVAL      dA4_DESTRD2  = %trim(%subst(vindta:158+1:40))
099200060227     C                   EVAL      dA4_DESTIND  = %trim(%subst(vindta:438+1:40))
099300060227     C                   EVAL      dA4_DESTCAP  = %trim(%subst(vindta:572+1:10))
099400060227     C                   EVAL      dA4_DESTLOC  = %trim(%subst(vindta:607+1:40))
099500060227     C                   EVAL      dA4_DESTNAZ  = %trim(%subst(vindta:937+1:3))
099600060227     C                   EVAL      dA4_DESTPRV  = %trim(%subst(vindta:943+1:3))
099700060227     C*
099800041210     C* ......VABRSD/VABRD2
099900041210     C                   MOVEL     *blanks       wRSD             70
100000041210     C                   EVAL      wRSD = %trim(%subst(vindta:119:40)) + ' ' +
100100041210     C                                    %trim(%subst(vindta:159:40)) + ' ' +
100200041210     C                                    %trim(%subst(vindta:199:40)) + ' ' +
100300041210     C                                    %trim(%subst(vindta:239:40))
100400041210     C                   EVAL      VABRSD=%subst(wRSD:1:35)
100500041210     C                   EVAL      VABRD2=%subst(wRSD:1+35:35)
100600041210     C* ......VABIND
100700041210     C                   EVAL      VABIND=%trim(%subst(vindta:439:40)) + ' ' +
100800041210     C                                    %trim(%subst(vindta:479:40)) + ' ' +
100900041210     C                                    %trim(%subst(vindta:519:40)) + ' ' +
101000041210     C                                    %trim(%subst(vindta:559:40))
101100041210     C* ......VABCAD
101200041210     C                   EVAL      PiStr=%trim(%subst(vindta:573:10))
101300041210     C                   EXSR      CHKNUM
101400041210     C                   IF        PiInt=*on
101500041210     C                   Z-ADD     PiVal         Num5_0
101600041210     C                   MOVEL(P)  Num5_0        VABCAD
101700041210     C                   ELSE
101800041210     C                   ADD       1             errore
101900041210     C                   EVAL      vinmsg = %trimr(vinmsg)
102000041210     C                             + ' ' + 'VABCAD'
102100041210     C                   ENDIF
102200041210     C* ......VABLOD
102300041210     C                   EVAL      VABLOD=%trim(%subst(vindta:608:40)) + ' ' +
102400041210     C                                    %trim(%subst(vindta:648:40))
102500041210     C* ......VATNOT
102600041210     C                   EVAL      VATNOT=%trim(%subst(vindta:748:30)) + ' ' +
102700041210     C                                    %trim(%subst(vindta:778:40))
102800041210     C                   EVAL      VATTRC = 'B'
102900041210     C* ......VABNZD (conversone da formato *ISO a formato "BARTOLINI"
103000041210     C                   EVAL      VABNZD=%trim(%subst(vindta:938:3))
103100041210     C                   Z-ADD     1             jNAZ
103200041210     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
103300041210     C                   IF        %found
103400041210     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
103500041210     C                   ENDIF
103600060225     C* ......VABPRD
103700041210     C                   EVAL      VABPRD=%trim(%subst(vindta:944:3))
103800040910     C*
103900040910     C                   ENDIF
104000040910     C*
104100040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
104200040910     C                   IF        FlgCAS <> '0'
104300040929     C                   IF        VABCBO = '1'
104400040910     C                   EVAL      VABCBO = '4'
104500040910     C                   ELSE
104600040929     C                   EVAL      VABCBO = '6'
104700040910     C                   ENDIF
104800040929     C                   ENDIF
104900040910     C*
105000040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
105100040910     C                   EXSR      CHKIMPDIV
105200010202     C*
105300000801     C* Ebbene...
105400000801     C                   ADD       1             §CTRMO
105500010201     C                   IF        errore <> *zeros
105600000801     C                   ADD       1             §CTRNO
105700000801     C                   EVAL      recko = vindta
105800000801     C                   EXCEPT    rigadett
105900000801     C                   EVAL      vinflg = '2'
106000000801     C                   ELSE
106100010201     C                   ADD       1             §CTROKVB
106200000801     C                   ENDIF
106300000801     C*
106400000801     C                   ENDSR
106500010202     C*----------------------------------------------------*
106600060225     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR E FILE FIVAXWWR
106700010202     C*----------------------------------------------------*
106800060225     C     PREVATVAX     BEGSR
106900010202     C*
107000060225     C* Compongo il nome del membro da dare al FIVATWWR e al FIVAXWWR
107100010202     C                   eval      parmbr = vlrhdl
107200010202     C                   movel     'M'           parmbr
107300041210     C                   eval      parccm = vlrksc
107400010202     C                   eval      paropz = '1'
107500010202     C* Effettuo la chiamata al CLLE preposto
107600060225     C                   call(e)   'TITVVTXC'
107700010202     C                   parm                    parccm
107800010202     C                   parm                    parmbr
107900010202     C                   parm                    paropz
108000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
108100010202     C                   if        %error
108200010202     C                   movel     '1'           chkcall
108300010202     C                   else
108400010202     C                   movel     '0'           chkcall
108500010202     C                   endif
108600010202     C*
108700010202     C                   ENDSR
108800000801     C*----------------------------------------------------*
108900000801     C*  CONTROLLO NUMERICITA' CAMPI
109000000801     C*----------------------------------------------------*
109100000801     C     CHKNUM        BEGSR
109200000801     C*
109300000801     C                   call(e)   'ISNUMERIC'
109400000801     C                   PARM                    PiStr            30
109500041210     C                   PARM      '.'           PiDecChr          1
109600000801     C                   PARM      *ZEROS        PiVal            30 9
109700000801     C                   PARM      '0'           PiInt             1
109800000801     C                   PARM      '0'           PiNum             1
109900000801     C                   IF        %error
110000060227     C                   EVAL      PiNum=*off
110100000801     C                   ENDIF
110200000801     C*
110300000801     C                   ENDSR
110400000801     C***
110500000801
110600011113
110700011113     C*----------------------------------------------------*
110800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
110900011113     C*----------------------------------------------------*
111000011113     C     CHKIMPDIV     BEGSR
111100011113     C*
111200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
111300011113     C                   Z-ADD     *zeros        wrkDec            9 9
111400011113     C*
111500011113     C* Come prima cosa effettuo considerazioni sulla divisa
111600011113     C                   IF        vabIAS > *zeros
111700011113     C                   IF        vabVAS <> 'EUR'
111800011113     C                   EVAL      vabVAS =  'ITL'
111900011113     C                   ENDIF
112000011113     C                   ENDIF
112100011113     C*
112200011113     C                   IF        vabCAS > *zeros
112300011113     C                   IF        vabVCA <> 'EUR'
112400011113     C                   EVAL      vabVCA =  'ITL'
112500011113     C                   ENDIF
112600011113     C                   ENDIF
112700011113     C*
112800011113     C                   IF        vabVMD > *zeros
112900020305     C                   IF        vabVAD <> 'EUR'
113000011113     C                   EVAL      vabVAD =  'ITL'
113100011113     C                   ENDIF
113200011113     C                   ENDIF
113300011113     C*
113400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
113500011113     C                   Z-ADD     vabIAS        wrkDec
113600011113     C                   IF        wrkDec > *zeros
113700011113     C                   IF        vabVAS = 'ITL'
113800011113     C                   EVAL      vabIAS = *zeros
113900011113     C                   ENDIF
114000011113     C                   ENDIF
114100011113     C*
114200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
114300011113     C                   Z-ADD     vabCAS        wrkDec
114400011113     C                   IF        wrkDec > *zeros
114500011113     C                   IF        vabVCA = 'ITL'
114600011113     C                   EVAL      vabCAS = *zeros
114700011113     C                   ENDIF
114800011113     C                   ENDIF
114900011113     C*
115000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
115100011113     C                   Z-ADD     vabVMD        wrkDec
115200011113     C                   IF        wrkDec > *zeros
115300011113     C                   IF        vabVAD = 'ITL'
115400011113     C                   EVAL      vabVMD = *zeros
115500011113     C                   ENDIF
115600011113     C                   ENDIF
115700011113     C*
115800011113     C                   ENDSR
115900011113     C***
116000011113
116100060227
116200060227
116300060227     C*----------------------------------------------------*
116400060227     C*  ROUTINE GENERAZIONE BUFFER FORMATI FILE FIVAXWWR
116500060227     C*----------------------------------------------------*
116600060227     C     WRIVAX        BEGSR
116700060227     C*
116800060227     C* Inizializzo contatore d riga e flag fine dati DDT x bolla corrente
116900060227     C                   Z-ADD     1             VAXPRR
117000060227     C                   MOVEL     '1'           wDDT              1
117100060228     C                   Z-ADD     *zeros        wTotVAX           5 0
117200060228     C*
117300060228     C* Valorizzo subito il totale d righe presenti nel DDT della bolla corrente
117400060228     C                   EVAL      wTotVAX = jVAR_PK + jVAR_RS + 35
117500060227     C*
117600060227     C* Imposto il tipo modulo x la stampa del logo personalizzato
117700060227     C                   EVAL      VAXTMD = 'BBRA'
117800060227     C*
117900060227     C* Ciclo o fino a fine dati memorizzati o cmq al massimo fino a 100 righe x bolla
118000060227     C                   DOW       VAXPRR <= 100 AND
118100060227     C                             wDDT    = '1'
118200060228     C*
118300060228     C* Se esaurite le righe da stampare esco dal ciclo
118400060228     C                   IF        VAXPRR > wTotVAX
118500060228     C                   LEAVE
118600060228     C                   ENDIF
118700060227     C*
118800060227     C* Innanzitutto inizializzo il campo dati flat
118900060227     C                   CLEAR                   VAXDTT
119000060227     C*
119100060227     C* A seconda della riga corrente effettuo specifiche considerazioni
119200060227     C                   IF        VAXPRR <= 5
119300060227     C                   EVAL      VAXTRK = 'T1'
119400060227     C                   ELSE
119500060228     C                   EVAL      VAXTRK = 'C1'
119600060227     C                   ENDIF
119700060227     C*
119800060227     C                   IF        VAXPRR = 1
119900060227     C                   RESET                   DS__T_1
120000060227     C                   EVAL      sT1_MITT = %trim(dA5_ORIGRSD)
120100060227     C                   EVAL      sT1_DEST = %trim(dA4_DESTRSD)
120200060227     C                   EVAL      VAXDTT   = DS__T_1
120300060227     C                   ENDIF
120400060227     C*
120500060227     C                   IF        VAXPRR = 2
120600060227     C                   RESET                   DS__T_1
120700060227     C                   EVAL      sT1_MITT = %trim(dA5_ORIGIND) + ' ' +
120800060227     C                                        %trim(dA5_ORIGCIV)
120900060227     C                   EVAL      sT1_DEST = %trim(dA4_DESTRD2)
121000060227     C                   EVAL      VAXDTT   = DS__T_1
121100060227     C                   ENDIF
121200060227     C*
121300060227     C                   IF        VAXPRR = 3
121400060227     C                   RESET                   DS__T_1
121500060227     C                   EVAL      sT1_MITT = %trim(dA5_ORIGCAP) + ' ' +
121600060227     C                                        %trim(dA5_ORIGLOC)
121700060227     C                   EVAL      sT1_DEST = %trim(dA4_DESTIND)
121800060227     C                   EVAL      VAXDTT   = DS__T_1
121900060227     C                   ENDIF
122000060227     C*
122100060227     C                   IF        VAXPRR = 4
122200060227     C                   RESET                   DS__T_1
122300060227     C                   EVAL      sT1_MITT = %trim(dA5_ORIGNAZD)
122400060227     C                   EVAL      sT1_DEST = %trim(dA4_DESTNAZ) + '-'  +
122500060227     C                                        %trim(dA4_DESTCAP) + ' '  +
122600060227     C                                        %trim(dA4_DESTLOC) + ' ' +
122700060227     C                                        '(' + %trim(dA4_DESTPRV) + ')'
122800060227     C                   EVAL      VAXDTT   = DS__T_1
122900060227     C                   ENDIF
123000060227     C*
123100060227     C                   IF        VAXPRR = 5
123200060227     C                   EVAL      VAXDTT   = *blanks
123300060227     C                   ENDIF
123400060227     C*
123500060227     C                   IF        VAXPRR = 6
123600060227     C                   RESET                   DS__T_2
123700060227     C                   EVAL      sT2_TES1_1 = 'Nr.Documento:'
123800060227     C                   EVAL      sT2_TES1_2 = %trim(dA8_NUMDOC)
123900060227     C                   EVAL      sT2_TES2_1 = 'Nr.Ordine vendita:'
124000060227     C                   EVAL      sT2_TES2_2 = %trim(dA1_NUMORD)
124100060227     C                   EVAL      VAXDTT   = DS__T_2
124200060227     C                   ENDIF
124300060227     C*
124400060227     C                   IF        VAXPRR = 7
124500060227     C                   RESET                   DS__T_2
124600060227     C                   EVAL      sT2_TES1_1 = 'Data spedizione:'
124700060227     C                   EVAL      sT2_TES1_2 = %trim(dA6_DATSPE)
124800060227     C                   EVAL      sT2_TES2_1 = 'Codice BBMI:'
124900060227     C                   EVAL      sT2_TES2_2 = %trim(dA4_DESTID)
125000060227     C                   EVAL      VAXDTT   = DS__T_2
125100060227     C                   ENDIF
125200060227     C*
125300060227     C                   IF        VAXPRR = 8
125400060227     C                   RESET                   DS__T_2
125500060227     C                   EVAL      sT2_TES1_1 = 'Itinerario:'
125600060227     C                   EVAL      sT2_TES1_2 = %trim(dA8_ROUTE)
125700060227     C                   EVAL      sT2_TES2_1 = 'Committente:'
125800060227     C                   EVAL      sT2_TES2_2 = %trim(dA3_MITTID)
125900060227     C                   EVAL      VAXDTT   = DS__T_2
126000060227     C                   ENDIF
126100060227     C*
126200060227     C                   IF        VAXPRR = 9
126300060227     C                   RESET                   DS__T_2
126400060227     C                   EVAL      sT2_TES1_1 = 'Invio:'
126500060227     C                   EVAL      sT2_TES2_1 = 'Nr.Ord. acq. com.:'
126600060227     C                   EVAL      sT2_TES2_2 = %trim(dB2_CLIORDNUM)
126700060227     C                   EVAL      VAXDTT   = DS__T_2
126800060227     C                   ENDIF
126900060227     C*
127000060227     C                   IF        VAXPRR = 10
127100060227     C                   RESET                   DS__T_2
127200060227     C                   EVAL      sT2_TES1_1 = 'Tipo porto:'
127300060227     C                   EVAL      sT2_TES1_2 = %trim(dA8_PORTO1) + ' '+
127400060227     C                                          %trim(dA8_PORTO2)
127500060227     C                   EVAL      sT2_TES2_1 = 'Data Ord.acq.com.:'
127600060228     C                   EVAL      sT2_TES2_2 = %trim(dB2_CLIORDDAT)
127700060227     C                   EVAL      VAXDTT   = DS__T_2
127800060227     C                   ENDIF
127900060227     C*
128000060227     C                   IF        VAXPRR = 11
128100060227     C                   RESET                   DS__T_2
128200060227     C                   EVAL      sT2_TES2_1 = 'Referente:'
128300060227     C                   EVAL      sT2_TES2_2 = %trim(dA2_CONTAT)
128400060227     C                   EVAL      VAXDTT   = DS__T_2
128500060227     C                   ENDIF
128600060227     C*
128700060227     C                   IF        VAXPRR = 12
128800060227     C                   RESET                   DS__T_2
128900060227     C                   EVAL      sT2_TES2_1 = 'Tel Ordine proc.:'
129000060227     C                   EVAL      sT2_TES2_2 = %trim(dA2_TEL)
129100060227     C                   EVAL      VAXDTT   = DS__T_2
129200060227     C                   ENDIF
129300060227     C*
129400060227     C                   IF        VAXPRR = 13
129500060227     C                   RESET                   DS__T_2
129600060227     C                   EVAL      sT2_TES2_1 = 'Fax Ordine proc.:'
129700060227     C                   EVAL      sT2_TES2_2 = %trim(dA2_FAX)
129800060227     C                   EVAL      VAXDTT   = DS__T_2
129900060227     C                   ENDIF
130000060227     C*
130100060227     C                   IF        VAXPRR = 14
130200060227     C                   EVAL      VAXDTT   = *blanks
130300060227     C                   ENDIF
130400060227     C*
130500060228     C                   IF        VAXPRR = 15
130600060227     C                   RESET                   DS__D_1
130700060227     C                   EVAL      VAXDTT   = DS__D_1
130800060227     C                   ENDIF
130900060227     C*
131000060228     C                   IF        VAXPRR = 16
131100060227     C                   RESET                   DS__D_2
131200060227     C                   EVAL      VAXDTT   = DS__D_2
131300060227     C                   ENDIF
131400060228     C*
131500060228     C                   IF        VAXPRR = 17
131600060228     C                   RESET                   DS__R_2
131700060228     C                   EVAL      VAXDTT   = DS__R_2
131800060228     C                   ENDIF
131900060227     C*
132000060228     C                   IF        VAXPRR = 18
132100060227     C                   RESET                   DS__D_3
132200060227     C                   EVAL      VAXDTT   = DS__D_3
132300060227     C                   ENDIF
132400060227     C*
132500060228     C                   IF        VAXPRR = 19
132600060227     C                   RESET                   DS__D_4
132700060227     C                   EVAL      sD4_MITT   = %trim(dA3_MITTRSD)
132800060227     C                   EVAL      sD4_VETT   = %trim(dA7_VETTRSD)
132900060227     C                   EVAL      VAXDTT   = DS__D_4
133000060227     C                   ENDIF
133100060227     C*
133200060228     C                   IF        VAXPRR = 20
133300060227     C                   RESET                   DS__D_4
133400060227     C                   EVAL      sD4_MITT   = %trim(dA3_MITTIND)
133500060227     C                   EVAL      sD4_VETT   = %trim(dA7_VETTIND)
133600060227     C                   EVAL      VAXDTT   = DS__D_4
133700060227     C                   ENDIF
133800060227     C*
133900060228     C                   IF        VAXPRR = 21
134000060227     C                   RESET                   DS__D_4
134100060227     C                   EVAL      sD4_MITT   = %trim(dA3_MITTCAP) + ' ' +
134200060227     C                                          %trim(dA3_MITTLOC)
134300060227     C                   EVAL      sD4_VETT   = %trim(dA7_VETTCAP) + ' ' +
134400060227     C                                          %trim(dA7_VETTLOC)
134500060227     C                   EVAL      VAXDTT   = DS__D_4
134600060227     C                   ENDIF
134700060227     C*
134800060228     C                   IF        VAXPRR = 22
134900060227     C                   EVAL      VAXDTT   = *blanks
135000060227     C                   ENDIF
135100060227     C*
135200060228     C                   IF        VAXPRR = 23
135300060227     C                   RESET                   DS__D_5
135400060227     C                   EVAL      sD5_DESCR  = 'Le seguenti posizioni '       +
135500060228     C                                      'articolo/i con Nr.ord. '          +
135600060227     C                                      %trim(dB5_IDOCNUM) + ', '          +
135700060228     C                                      'Nr.ord. acquisto '                +
135800060227     C                                      %trim(dB2_CLIORDNUM) + ', '        +
135900060228     C                                      'Data ord. acquisto '              +
136000060227     C                                      %trim(dB2_CLIORDDAT)
136100060228     C                   EVAL      VAXDTT   = DS__D_5
136200060227     C                   ENDIF
136300060227     C*
136400060228     C                   IF        VAXPRR = 24
136500060227     C                   RESET                   DS__D_6
136600060227     C                   EVAL      sD6_POSNUM = %trim(dB6_DETPOSNUM)
136700060227     C                   EVAL      sD6_MATNUM = %trim(dB6_DETMATNUM)
136800060227     C                   EVAL      sD6_QTA    = '???'
136900060228     C                   EVAL      sD6_QTAORD = %trim(dB1_QTAORD) + ' ' +
137000060228     C                                          %trim(dB1_UNIT)
137100060228     C                   EVAL      sD6_QTARES = %trim(dB1_QTADIF) + ' ' +
137200060228     C                                          %trim(dB1_UNIT)
137300060227     C                   EVAL      VAXDTT   = DS__D_6
137400060228     C                   ENDIF
137500060228     C*
137600060228     C                   IF        VAXPRR = 25
137700060228     C                   EVAL      VAXDTT   = *blanks
137800060228     C                   ENDIF
137900060228     C*
138000060228     C                   IF        VAXPRR = 26
138100060227     C*
138200060227     C* Se presenti informazioni sul "Residuo da evadere" => gestisco....
138300060227     C                   IF        jVAR_RS > *zeros
138400060228     C* ...titolo  (riga 26)
138500060227     C                   RESET                   DS__E_1
138600060227     C                   EVAL      VAXDTT   = DS__E_1
138700060228     C                   EVAL      VAXTRK = 'T1'                                * vado in grassetto
138800060227     C                   WRITE     FIVAX000
138900060228     C                   ADD       1             VAXPRR
139000060228     C                   EVAL      VAXTRK = 'C1'                                * torno a normale
139100060228     C* ...intestazioni colonne (riga 27)
139200060227     C                   RESET                   DS__E_2
139300060227     C                   EVAL      VAXDTT   = DS__E_2
139400060227     C                   WRITE     FIVAX000
139500060228     C                   ADD       1             VAXPRR
139600060228     C* ...riga separazione "alta" (riga 28)
139700060227     C                   RESET                   DS__R_2
139800060227     C                   EVAL      VAXDTT   = DS__R_2
139900060227     C                   WRITE     FIVAX000
140000060228     C                   ADD       1             VAXPRR
140100060228     C* ...ciclo su dettaglio (<n> righe)
140200060228     C                   Z-ADD     *zeros        jWRK              3 0
140300060228     C                   DO        *hival
140400060228     C*
140500060228     C* Innanzitutto testo il contatore righe stampate e se > 100 => esco dal ciclo....
140600060228     C                   IF        VAXPRR > 100
140700060228     C                   LEAVE
140800060228     C                   ENDIF
140900060228     C*
141000060228     C* Incremento l'indice della schiera corrente
141100060228     C                   ADD       1             jWRK
141200060228     C*
141300060228     C* Finchè memorizzato qualcosa...  altrimenti esco dal ciclo
141400060228     C                   IF        skVAR_RS(jWRK) <> *blanks
141500060228     C                   EVAL      wDS_A1 = skVAR_RS(jWRK)
141600060228     C                   RESET                   DS__E_3
141700060228     C                   EVAL      sE3_NUMORD = %trim(wdA1_NUMORD)
141800060228     C                   EVAL      sE3_POS    = %trim(wdA1_POS)
141900060228     C                   EVAL      sE3_MATNUM = %trim(wdA1_MATNUM)
142000060228     C                   EVAL      sE3_MATDES = %trim(wdA1_MATDES)
142100060228     C                   EVAL      sE3_QTAORD = %trim(wdA1_QTAORD) + ' ' +
142200060228     C                                          %trim(wdA1_UNIT)
142300060228     C                   EVAL      sE3_QTADIF = %trim(wdA1_QTADIF) + ' ' +
142400060228     C                                          %trim(wdA1_UNIT)
142500060228     C                   EVAL      VAXDTT   = DS__E_3
142600060228     C*
142700060228     C* Quindi scarico il buffer della riga corrente del DDT bolla corrente
142800060228     C                   WRITE     FIVAX000
142900060228     C*
143000060228     C* Incremento il contatore d riga corrente
143100060228     C                   ADD       1             VAXPRR
143200060228     C*
143300060228     C                   ELSE
143400060228     C*
143500060228     C* Al primo elemento d schiera nn valorizzato => esco dal ciclo...
143600060228     C                   LEAVE
143700060228     C                   ENDIF
143800060227     C*
143900060227     C                   ENDDO
144000060228     C                   ENDIF
144100060228     C                   ENDIF
144200060228     C*
144300060228     C                   IF        VAXPRR = 36
144400060228     C                   RESET                   DS__R_1
144500060228     C                   EVAL      VAXDTT   = DS__R_1
144600060228     C                   ENDIF
144700060228     C*
144800060228     C                   IF        VAXPRR = 37
144900060228     C                   RESET                   DS__R_2
145000060228     C                   EVAL      VAXDTT   = DS__R_2
145100060228     C                   ENDIF
145200060228     C*
145300060228     C                   IF        VAXPRR = 38
145400060228     C                   RESET                   DS__P_1
145500060228     C                   EVAL      sP1_DICITUR1='Peso totale:'
145600060228     C                   EVAL      sP1_VALORE1 =%trim(dA8_PESOTOT) + ' ' +
145700060228     C                                          %trim(dA8_PESOUNIT)
145800060228     C                   EVAL      sP1_DICITUR2='Numero colli:'
145900060228     C                   EVAL      sP1_VALORE2 =%trim(dA8_COLLI)
146000060228     C                   EVAL      VAXDTT   = DS__P_1
146100060228     C                   ENDIF
146200060228     C*
146300060228     C                   IF        VAXPRR = 39
146400060228     C                   RESET                   DS__P_1
146500060228     C                   EVAL      sP1_DICITUR1='Peso netto:'
146600060228     C                   EVAL      sP1_VALORE1 =%trim(dA8_PESONET) + ' ' +
146700060228     C                                          %trim(dA8_PESOUNIT)
146800060228     C                   EVAL      VAXDTT   = DS__P_1
146900060228     C                   ENDIF
147000060228     C*
147100060228     C                   IF        VAXPRR = 40
147200060228     C                   RESET                   DS__P_1
147300060228     C                   EVAL      sP1_DICITUR1='Volume totale:'
147400060228     C                   EVAL      sP1_VALORE1 =%trim(dA8_VOLTOT) + ' ' +
147500060228     C                                          %trim(dA8_VOLUNIT)
147600060228     C                   EVAL      sP1_DICITUR2='Conferma ricevimento (Data e '+
147700060228     C                                          'firma):'
147800060228     C                   EVAL      VAXDTT   = DS__P_1
147900060228     C                   ENDIF
148000060228     C*
148100060228     C                   IF        VAXPRR = 41
148200060228     C                   RESET                   DS__R_1
148300060228     C                   EVAL      VAXDTT   = DS__R_1
148400060228     C                   ENDIF
148500060227     C*
148600060227     C* Quindi scarico il buffer della riga corrente del DDT bolla corrente
148700060227     C                   WRITE     FIVAX000
148800060227     C*
148900060227     C* Incremento il contatore d riga corrente
149000060227     C                   ADD       1             VAXPRR
149100060227     C*
149200060227     C* Proseguo con la generazione dei dati x stampa DDT
149300060227     C                   ENDDO
149400060227     C*
149500060227     C                   ENDSR
149600011113
149700000801
149800000801
149900990920      /TITLE Invio dei dati al punto operativo.
150000010202     C     invio         BEGSR
150100060225     C*
150200060225     C* 1° invio FIVAX
150300060225     C                   reset                   dscmz
150400060225     C                   move      vlrpoi        cmzdst
150500060225     C                   eval      cmzfld = 'FIVAXWWR'
150600060225     C                   eval      cmzmbd = vlrhdl
150700060225     C                   eval      %subst(cmzmbd:1:1) = 'M'
150800060225     C***                if        prmfir = *blanks
150900060225     C                   eval      cmzfla = 'FIVAX00F'
151000060225     C                   eval      cmzmba = 'FIVAX00F'
151100060225     C***                else
151200060225     C***                eval      cmzfla = prmfir
151300060225     C***                eval      cmzmba = prmfir
151400060225     C***                endif
151500060225     C                   eval      cmznrr = *zeros
151600060227     C                   move      §ctrokvx      cmznrr
151700060225     C                   eval      cmzlba = vlrfl1
151800060225     C                   call(e)   'TIS711C'
151900060225     C                   parm                    dscmz
152000060225     C                   parm      *blanks       esito
152100060225     C                   if        %error
152200060225     C                             or cmzerr = '1'
152300060225     C                             or esito  = '1'
152400060225     C                   eval      wrkesito = '3'
152500060225     C                   else
152600990920     C*
152700060225     C* 2° invio FIVAT
152800010201     C                   reset                   dscmz
152900010201     C                   move      vlrpoi        cmzdst
153000021113     C                   eval      cmzfld = 'FIVATWWR'
153100010201     C                   eval      cmzmbd = vlrhdl
153200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
153300021009     C***                if        prmfir = *blanks
153400021113     C                   eval      cmzfla = 'FIVAT00F'
153500021113     C                   eval      cmzmba = 'FIVAT00F'
153600021009     C***                else
153700021009     C***                eval      cmzfla = prmfir
153800021009     C***                eval      cmzmba = prmfir
153900021009     C***                endif
154000010201     C                   eval      cmznrr = *zeros
154100020305     C                   move      §ctrokvt      cmznrr
154200021018     C                   eval      cmzlba = vlrfl1
154300010201     C                   call(e)   'TIS711C'
154400010201     C                   parm                    dscmz
154500010201     C                   parm      *blanks       esito
154600010205     C                   if        %error
154700010205     C                             or cmzerr = '1'
154800010205     C                             or esito  = '1'
154900010205     C                   eval      wrkesito = '3'
155000010205     C                   else
155100010201     C*
155200060225     C* 3° invio FIVAB
155300010201     C                   reset                   dscmz
155400010201     C                   move      vlrpoi        cmzdst
155500010201     C                   eval      cmzfld = vlrfou
155600010201     C                   eval      cmzmbd = vlrhdl
155700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
155800021009     C***                if        prmfir = *blanks
155900021113     C                   eval      cmzfla = 'FIVAB00F'
156000021113     C                   eval      cmzmba = 'FIVAB00F'
156100021009     C***                else
156200021009     C***                eval      cmzfla = prmfir
156300021009     C***                eval      cmzmba = prmfir
156400021009     C***                endif
156500010201     C                   eval      cmznrr = *zeros
156600010201     C                   move      §ctrokvb      cmznrr
156700021018     C                   eval      cmzlba = vlrfl1
156800010201     C                   call(e)   'TIS711C'
156900010201     C                   parm                    dscmz
157000010201     C                   parm      *blanks       esito
157100010201     C                   if        %error
157200010201     C                             or cmzerr = '1'
157300010201     C                             or esito  = '1'
157400010201     C                   eval      wrkesito = '3'
157500010201     C                   endif
157600010205     C                   endif
157700060225     C                   endif
157800990920     C*
157900000613     C                   ENDSR
158000000613     C***
158100990910
158200000613     C     *inzsr        BEGSR
158300990910     C*
158400990910     C     *entry        plist
158500990920     C                   parm                    tivlrds
158600990921     C                   parm      wrkesito      esito
158700000724     C                   parm                    prmlit
158800000710     C                   parm                    prmfir
158900000613     C*
159000000830     C* CALCOLA LA DATA CORRENTE
159100000830     C                   time                    wn14             14 0
159200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
159300000830     C                   z-add     wn8           g08dat
159400000830     C                   z-add     *zeros        g08inv
159500000830     C                   movel     '0'           g08err
159600000830     C                   call      'XSRDA8'
159700000830     C                   parm                    wlbda8
159800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
159900041210     C*
160000041210     C* Chiave su TABEL00F - parziale
160100041210     C     KEYtabP       KLIST
160200041210     C                   KFLD                    tblKUT
160300041210     C                   KFLD                    tblCOD
160400000830     C*
160500000613     C                   ENDSR
160600000613     C***
160700990908
160800041206     Otitvg4p   E            riepilogo         2
160900990915     O                                              'Upload via Internet'
161000990915     O                                           +1 'Traduzione TIVIN00R -'
161100060225     O                                           +1 'FIVAB/FIVAT/FIVAX'
161200010201     O                                           +1 'Report testate bolle'
161300990915     O          E            riepilogo   2
161400990915     O                       wrkdata
161500990915     O                       wrkora              +1
161600990915     O                       procname            +1
161700990915     O          E            riepilogo   2
161800990915     O                                              'Cliente..................:'
161900990915     O                       VABCCM        z     +1
162000990915     O          E            riepilogo   2
162100990920     O                                              'Riferimento Strategi.....:'
162200990920     O                       vlrhdl              +1
162300990915     O          E            riepilogo   2
162400990915     O                                              'Giusti...................:'
162500010201     O                       §CTROKVB      2   +  1
162600990915     O          E            riepilogo   2
162700990915     O                                              'Sbagliati e corretti.....:'
162800971022     O                       §CTRMO        2   +  1
162900990915     O          E            riepilogo   2
163000990915     O                                              'Sbagliati e scartati.....:'
163100971022     O                       §CTRNO        2   +  1
163200000613
163300041206     Otitvg4ps  E            testdett          2
163400000613     O                                              'Upload via Internet'
163500000613     O                                           +1 'Traduzione TIVIN00R -'
163600060225     O                                           +1 'FIVAB/FIVAT/FIVAX'
163700010201     O                                           +1 'Report testate bolle'
163800000616     O          E            testdett    3
163900000613     O                                           +2 'N° rec'
164000000613     O                                           +3 'Anteprima contenuto'
164100000616     O          E            rigadett    2
164200000613     O                       rrnum               +2
164300000621     O                       recko               +3
164400000616     O          E            findett     2
164500000613     O                       wrkdata
164600000613     O                       wrkora              +1
164700000613     O                       procname            +1
164800000616     O          E            findett     2
164900000613     O                                              'Cliente..................:'
165000000613     O                       VABCCM        z     +1
165100000616     O          E            findett     2
165200000613     O                                              'Riferimento Strategi.....:'
165300000613     O                       vlrhdl              +1
165400000616     O          E            findett     2
165500000613     O                                              'Giusti...................:'
165600010201     O                       §CTROKVB      2   +  1
165700000616     O          E            findett     2
165800000613     O                                              'Sbagliati e corretti.....:'
165900000613     O                       §CTRMO        2   +  1
166000000616     O          E            findett     2
166100000613     O                                              'Sbagliati e scartati.....:'
166200000613     O                       §CTRNO        2   +  1
166300000512** CMD - COMANDI CL
166400041206OVRPRTF FILE(TITVG4P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
166500041206OVRPRTF FILE(TITVG4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
166600041206DLTOVR FILE(TITVG4P TITVG4PS) LVL(*)
166700000512
166800000512
