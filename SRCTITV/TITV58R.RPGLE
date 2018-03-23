000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800030620     Ftitv58p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100030620     Ftitv58ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200030620     D fnvabaar      e ds                  inz
002300990920     D dscmz         e ds                  inz
002400990910     D psds           sds
002500990910     D  procname         *PROC
002600990920     D tivlrds       e ds                  extname(tivlr00f)
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600030415     D depspe          s              7    INZ(*blanks)
003700010202     D parccm          s              8    INZ(*blanks)
003800010202     D parmbr          s             10    INZ(*blanks)
003900010202     D paropz          s              1    INZ(*blanks)
004000010202     D chkcall         s              1    INZ(*blanks)
004100010213
004200000801     D  Num5_0         s              5  0
004300030107     D  Num7_0         s              7  0
004400030620
004500030620     D*------------------
004600030620     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
004700030620     D*------------------
004800030620     D* 10+3
004900030620     D                 DS
005000030620     D  WS15                   1      1
005100030620     D  WI15                   2     11
005200030620     D  WV15                  12     12
005300030620     D  WD15                  13     15
005400030620     D  WASC15                 1     15
005500030620     D*  6+1
005600030620     D                 DS
005700030620     D  WS09                   1      1
005800030620     D  WI09                   2      7
005900030620     D  WV09                   8      8
006000030620     D  WD09                   9      9
006100030620     D  WASC09                 1      9
006200030620     D*  2+3
006300030620     D                 DS
006400030620     D  WS07                   1      1
006500030620     D  WI07                   2      3
006600030620     D  WV07                   4      4
006700030620     D  WD07                   5      7
006800030620     D  WASC07                 1      7
006900000830
007000000830     D*------------------
007100000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
007200000830     D*------------------
007300000830     D WLBDA8          DS                  INZ
007400000830     D  G08DAT                 1      8  0
007500000830     D  G08INV                 9     16  0
007600000830     D  G08ERR                17     17
007700000830     D  G08TGI                18     22  0
007800000830     D*
007900990908
008000010201
008100010201
008200990915     C                   time                    wrkdata
008300990915     C                   time                    wrkora
008400000913     C                   reset                   rrnum
008500990921     C                   reset                   esito
008600990921     C                   reset                   wrkesito
008700000724     C*
008800000724     C* SE OCCORRE SPEDIRE IN FILIALE
008900021017     C                   if        vlrpoi <> *zeros
009000000724     C*
009100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009300021017     C     vlrpoi        chain     azorg01l
009400000724     C                   if        %found
009500000616     C                   movel(p)  CMD(1)        depcmd
009600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009700000616     C*
009800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009900011113     C                   Z-ADD     150           LENGH            15 5
010000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010100000616     C                   PARM                    depcmd
010200000616     C                   PARM                    LENGH
010300000724     C*
010400000724     C                   endif
010500000724     C                   endif
010600000616     C*
010700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010800000616     C                   movel(p)  CMD(2)        depcmd
010900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011000000616     C*
011100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011200011113     C                   Z-ADD     150           LENGH            15 5
011300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011400000616     C                   PARM                    depcmd
011500000616     C                   PARM                    LENGH
011600000616     C*
011700030620     C                   if        not %open(titv58ps)
011800030620     C                   open      titv58ps
011900000616     C                   except    testdett
012000000613     C                   endif
012100000613     C*
012200030620     C                   EXSR      RWVAB                                        LETT/SCR. VAB
012300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012400000613     C*
012500010202     C* Effettuo la chiamata al CLLE preposto
012600030620     C                   call(e)   'TITVVTC'
012700010202     C                   parm                    parccm
012800010202     C                   parm                    parmbr
012900010202     C                   parm      '2'           paropz
013000010202     C*
013100030620     C                   if        %open(titv58ps)
013200000616     C                   except    findett
013300030620     C                   close     titv58ps
013400000613     C                   endif
013500000616     C*
013600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013700011113     C                   Z-ADD     150           LENGH            15 5
013800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013900000616     C                   PARM                    CMD(3)
014000000616     C                   PARM                    LENGH
014100000616     C*
014200010201     C                   seton                                        LR
014300000613
014400000613
014500000613     C*--------------------------------------------------------
014600000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014700000613     C*--------------------------------------------------------
014800000621     C     STPR          BEGSR
014900000613     C*
015000030620     C                   if        not %open(titv58p)
015100030620     C                   open      titv58p
015200990915     C                   endif
015300990915     C*
015400990915     C                   except    riepilogo
015500990915     C*
015600030620     C                   if        %open(titv58p)
015700030620     C                   close     titv58p
015800990914     C                   endif
015900990910     C*
016000000613     C                   ENDSR
016100000613     C***
016200990908
016300000801
016400910830     C*--------------------------------------------------------
016500030620     C* RWVAB LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
016600910830     C*--------------------------------------------------------
016700030620     C     RWVAB         BEGSR
016800990910     C*
016900990914     C                   if        not %open(tivin00r)
017000990908     C                   open      tivin00r
017100990914     C                   endif
017200021017     C                   if        not %open(fivabwwr)
017300021017     C                   open      fivabwwr
017400990914     C                   endif
017500021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017600020305     C                   exsr      prevat
017700010201     C*
017800010202     C                   if        chkcall = '0'
017900010202     C*
018000021017     C                   if        not %open(fivatwwr)
018100021017     C                   open      fivatwwr
018200010201     C                   endif
018300990910     C*
018400010201     C                   clear                   §CTROKVB          5 0
018500020305     C                   clear                   §CTROKVT          5 0
018600000801     C                   clear                   §CTRMO            5 0
018700000801     C                   clear                   §CTRNO            5 0
018800990910     C*
018900921023     C                   DO        *HIVAL
019000990913     C*
019100990915     C                   READ      tivin00r                               70
019200030620     C*
019300030620     C* Chiodo x evitare segnalazioni di errore inconsistenti
019400030620     C                   if        %subst(vindta:1:1) = '='
019500030620     C                   eval      %subst(vindta:1:1) = *blanks
019600030620     C                   endif
019700030620     C*
019800000905     C                   if        vindta > *blanks
019900000613     C                   add       1             rrnum
020000030620     C* ...elimino eventuali segni
020100030620     C     '+':' '       xlate     vindta        vindta
020200000801     C*
020300000801     C                   if        *in70 = *off
020400000801     C                             and
020500000801     C                             (vinflg = *blanks
020600000801     C                              or vinflg = '0'
020700000801     C                              or vinflg = '2')
020800000801     C*
020900000801     C                   clear                   vinmsg
021000030310     C                   eval      vinflg = '1'
021100030620     C*
021200030506     C                   clear                   fivab000
021300030506     C                   clear                   fivat000
021400030506     C                   exsr      impvab                                       => carico  VAB
021500030623     C*
021600030623     C* Se richiesto dettaglio colli (Disk "C") scrivo anche il VAT
021700030623     C                   if        wFlgDkC = '1'
021800030620     C                   exsr      impvat                                       => carico  VAT
021900030623     C                   endif
022000030506     C                   exsr      wrivab                                       => scarico VAB
022100000905     C*
022200000905     C                   else
022300000905     C                   eval      vinflg = '1'
022400000905     C                   endif
022500030411     C                   endif
022600000905     C*
022700000905     C  N70              update    tivin000
022800000905     C*
022900991022     C  N70              ENDdo
023000010202     C*
023100010202     C                   endif
023200990910
023300990910     C* Se non ci sono record con errori ...
023400000710     C                   if        §ctrno = 0
023500990910     C* ... restituisco esito OK.
023600990921     C                   eval      wrkesito = '0'
023700990910     C                   else
023800010201     C                   if        §ctrokvb > 0
023900990921     C                   eval      wrkesito = '1'
024000000710     C                   else
024100000710     C                   eval      wrkesito = '2'
024200990910     C                   endif
024300000710     C                   endif
024400990910     C*
024500990914     C                   if        %open(tivin00r)
024600990908     C                   close     tivin00r
024700990914     C                   endif
024800021017     C                   if        %open(fivabwwr)
024900021017     C                   close     fivabwwr
025000990914     C                   endif
025100021017     C                   if        %open(fivatwwr)
025200021017     C                   close     fivatwwr
025300010201     C                   endif
025400021017     C*
025500021017     C                   if        vlrpoi <> 999
025600021017     C                   eval      vlrpoi = vlrpoi
025700021017     C                   endif
025800990910     C*
025900010201     C                   if        §ctrokvb > 0
026000021017     C                             and vlrpoi > *zeros
026100010202     C                   exsr      invio
026200990920     C                   endif
026300990920     C*
026400910830     C                   ENDSR
026500000613     C***
026600990920
026700000801     C*----------------------------------------------------*
026800030415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
026900000801     C*----------------------------------------------------*
027000010201     C     IMPVAB        BEGSR
027100030620     C*
027200030620     C                   eval      fnvabaar = vindta
027300030620     C*
027400030620     C                   testb     x'3F'         vhbatb                   02
027500030620     C                   if        not *in02
027600030620     C*
027700030620     C                   SETOFF                                       303132
027800030620     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
027900030620     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
028000030620     C*    OPPURE TUTTI BLANKS
028100030620     C*
028200030620     C                   TESTN                   VHBLNP               30
028300030620     C  N30              TESTN                   VHBLNP                 30
028400030620     C  N30              TESTN                   VHBLNP                   30
028500030620     C                   MOVE      VHBLNP        W001A             1
028600030620     C     *IN30         IFEQ      *OFF
028700030620     C     W001A         ORLT      '0'
028800030620     C                   SETON                                        31
028900030620     C                   eval      vinmsg = %trimr(vinmsg)
029000030620     C                             + ' ' + 'VHBLNP'
029100030620     C                   ELSE
029200030620     C                   MOVE      VHBLNP        VABLNP
029300030620     C     VABLNP        IFLE      *ZEROS
029400030620     C                   SETON                                        31
029500030620     C                   eval      vinmsg = %trimr(vinmsg)
029600030620     C                             + ' ' + 'VHBLNP'
029700030620     C                   ENDIF
029800030620     C                   ENDIF
029900030620     C*
030000030620     C                   MOVE(P)   vlrpoi        VABFGS
030100030620     C*
030200030620     C                   TESTN                   VHBCCM               30
030300030620     C  N30              TESTN                   VHBCCM                 30
030400030620     C  N30              TESTN                   VHBCCM                   30
030500030620     C                   MOVE      VHBCCM        W001A             1
030600030620     C     *IN30         IFEQ      *OFF
030700030620     C     W001A         ORLT      '0'
030800030620     C                   SETON                                        31
030900030620     C                   eval      vinmsg = %trimr(vinmsg)
031000030620     C                             + ' ' + 'VHBCCM'
031100030620     C                   ELSE
031200030620     C                   MOVE      VHBCCM        VABCCM
031300030620     C     VABCCM        IFLE      *ZEROS
031400030620     C                   SETON                                        31
031500030620     C                   eval      vinmsg = %trimr(vinmsg)
031600030620     C                             + ' ' + 'VHBCCM'
031700030620     C                   ENDIF
031800030620     C                   ENDIF
031900030620     C*
032000030620     C                   TESTN                   VHBAAS               30
032100030620     C  N30              TESTN                   VHBAAS                 30
032200030620     C  N30              TESTN                   VHBAAS                   30
032300030620     C                   MOVE      VHBAAS        W001A             1
032400030620     C     *IN30         IFEQ      *OFF
032500030620     C     W001A         ORLT      '0'
032600030620     C     W001A         ANDNE     *BLANK
032700030620     C                   SETON                                        32
032800030620     C                   MOVEL     datcor        VABAAS
032900030620     C                   eval      vinmsg = %trimr(vinmsg)
033000030620     C                             + ' ' + 'VHBAAS'
033100030620     C                   ELSE
033200030620     C                   MOVE      VHBAAS        VABAAS
033300030620     C                   ENDIF
033400030620     C*
033500030620     C                   TESTN                   VHBMGS               30
033600030620     C  N30              TESTN                   VHBMGS                 30
033700030620     C  N30              TESTN                   VHBMGS                   30
033800030620     C                   MOVE      VHBMGS        W001A             1
033900030620     C     *IN30         IFEQ      *OFF
034000030620     C     W001A         ORLT      '0'
034100030620     C     W001A         ANDNE     *BLANK
034200030620     C                   SETON                                        32
034300030620     C                   MOVE      '0101'        VABMGS
034400030620     C                   eval      vinmsg = %trimr(vinmsg)
034500030620     C                             + ' ' + 'VHBMGS'
034600030620     C                   ELSE
034700030620     C                   MOVE      VHBMGS        VABMGS
034800030620     C                   ENDIF
034900030620     C*
035000030620     C                   TESTN                   VHBNRS               30
035100030620     C  N30              TESTN                   VHBNRS                 30
035200030620     C  N30              TESTN                   VHBNRS                   30
035300030620     C                   MOVE      VHBNRS        W001A             1
035400030620     C     *IN30         IFEQ      *OFF
035500030620     C     W001A         ORLT      '0'
035600030620     C     W001A         ANDNE     *BLANK
035700030620     C                   SETON                                        32
035800030620     C                   MOVE      *ZERO         VABNRS
035900030620     C                   eval      vinmsg = %trimr(vinmsg)
036000030620     C                             + ' ' + 'VHBNRS'
036100030620     C                   ELSE
036200030620     C                   MOVE      VHBNRS        VABNRS
036300030620     C                   ENDIF
036400030620     C*
036500030620     C                   TESTN                   VHBLNA               30
036600030620     C  N30              TESTN                   VHBLNA                 30
036700030620     C  N30              TESTN                   VHBLNA                   30
036800030620     C                   MOVE      VHBLNA        W001A             1
036900030620     C     *IN30         IFEQ      *OFF
037000030620     C     W001A         ORLT      '0'
037100030620     C     W001A         ANDNE     *BLANK
037200030620     C                   SETON                                        32
037300030620     C                   MOVE      *ZERO         VABLNA
037400030620     C                   eval      vinmsg = %trimr(vinmsg)
037500030620     C                             + ' ' + 'VHBLNA'
037600030620     C                   ELSE
037700030620     C                   MOVE      VHBLNA        VABLNA
037800030620     C                   ENDIF
037900030620     C*
038000030620     C                   TESTN                   VHBCTR               30
038100030620     C  N30              TESTN                   VHBCTR                 30
038200030620     C  N30              TESTN                   VHBCTR                   30
038300030620     C                   MOVE      VHBCTR        W001A             1
038400030620     C     *IN30         IFEQ      *OFF
038500030620     C     W001A         ORLT      '0'
038600030620     C     W001A         ANDNE     *BLANK
038700030620     C                   SETON                                        32
038800030620     C                   MOVE      *ZERO         VABCTR
038900030620     C                   eval      vinmsg = %trimr(vinmsg)
039000030620     C                             + ' ' + 'VHBCTR'
039100030620     C                   ELSE
039200030620     C                   MOVE      VHBCTR        VABCTR
039300030620     C                   ENDIF
039400030620     C*
039500030620     C                   MOVE      *ZERO         VABIAS
039600030620     C                   MOVE      VHBIAS        WASC15
039700030620     C                   TESTN                   WI15                 30
039800030620     C  N30              TESTN                   WI15                   30
039900030620     C  N30              TESTN                   WI15                     30
040000030620     C                   MOVE      WI15          W001A             1
040100030620     C     *IN30         IFEQ      *OFF
040200030620     C     W001A         ORLT      '0'
040300030620     C     W001A         ANDNE     *BLANK
040400030620     C                   SETON                                        32
040500030620     C                   MOVE      *ZERO         VABIAS
040600030620     C                   eval      vinmsg = %trimr(vinmsg)
040700030620     C                             + ' ' + 'VHBIAS'
040800030620     C                   ELSE
040900030620     C                   MOVEL     WI15          VABIAS
041000030620     C                   ENDIF
041100030620     C                   TESTN                   WD15                 30
041200030620     C  N30              TESTN                   WD15                   30
041300030620     C  N30              TESTN                   WD15                     30
041400030620     C                   MOVE      WD15          W001A             1
041500030620     C     *IN30         IFEQ      *OFF
041600030620     C     W001A         ORLT      '0'
041700030620     C     W001A         ANDNE     *BLANK
041800030620     C                   SETON                                        32
041900030620     C                   MOVE      *ZERO         VABIAS
042000030620     C                   eval      vinmsg = %trimr(vinmsg)
042100030620     C                             + ' ' + 'VHBIAS'
042200030620     C                   ELSE
042300030620     C                   MOVE      WD15          VABIAS
042400030620     C                   ENDIF
042500030620     C*
042600030620     C                   TESTN                   VHBNCL               30
042700030620     C  N30              TESTN                   VHBNCL                 30
042800030620     C  N30              TESTN                   VHBNCL                   30
042900030620     C                   MOVE      VHBNCL        W001A             1
043000030620     C     *IN30         IFEQ      *OFF
043100030620     C     W001A         ORLT      '0'
043200030620     C                   SETON                                        32
043300030620     C                   MOVE      *ZERO         VABNCL
043400030620     C                   eval      vinmsg = %trimr(vinmsg)
043500030620     C                             + ' ' + 'VHBNCL'
043600030620     C                   ELSE
043700030620     C                   MOVE      VHBNCL        VABNCL
043800030620     C                   ENDIF
043900030620     C*
044000030620     C                   MOVE      *ZERO         VABPKB
044100030620     C                   MOVE      VHBPKB        WASC09
044200030620     C                   IF        WI09 = *blanks
044300030620     C                   EVAL      WI09 = *zeros
044400030620     C                   ENDIF
044500030620     C                   TESTN                   WI09                 30
044600030620     C  N30              TESTN                   WI09                   30
044700030620     C  N30              TESTN                   WI09                     30
044800030620     C                   MOVE      WI09          W001A             1
044900030620     C     *IN30         IFEQ      *OFF
045000030620     C     W001A         ORLT      '0'
045100030620     C                   SETON                                        32
045200030620     C                   MOVE      *ZERO         VABPKB
045300030620     C                   eval      vinmsg = %trimr(vinmsg)
045400030620     C                             + ' ' + 'VHBPKB'
045500030620     C                   ELSE
045600030620     C                   MOVEL     WI09          VABPKB
045700030620     C                   ENDIF
045800030620     C                   IF        WD09 = *blanks
045900030620     C                   EVAL      WD09 = *zeros
046000030620     C                   ENDIF
046100030620     C                   TESTN                   WD09                 30
046200030620     C  N30              TESTN                   WD09                     30
046300030620     C                   MOVE      WD09          W001A             1
046400030620     C     *IN30         IFEQ      *OFF
046500030620     C     W001A         ORLT      '0'
046600030620     C     W001A         ANDNE     *BLANK
046700030620     C                   SETON                                        32
046800030620     C                   MOVE      *ZERO         VABPKB
046900030620     C                   eval      vinmsg = %trimr(vinmsg)
047000030620     C                             + ' ' + 'VHBPKB'
047100030620     C                   ELSE
047200030620     C                   MOVE      WD09          VABPKB
047300030620     C                   ENDIF
047400030620     C*
047500030620     C                   MOVE      *ZERO         VABVLB
047600030620     C                   MOVE      VHBVLB        WASC07
047700030620     C                   IF        WI07 = *blanks
047800030620     C                   EVAL      WI07 = *zeros
047900030620     C                   ENDIF
048000030620     C                   TESTN                   WI07                 30
048100030620     C  N30              TESTN                   WI07                   30
048200030620     C  N30              TESTN                   WI07                     30
048300030620     C                   MOVE      WI07          W001A             1
048400030620     C     *IN30         IFEQ      *OFF
048500030620     C     W001A         ORLT      '0'
048600030620     C     W001A         ANDNE     *BLANK
048700030620     C                   SETON                                        32
048800030620     C                   MOVE      *ZERO         VABVLB
048900030620     C                   eval      vinmsg = %trimr(vinmsg)
049000030620     C                             + ' ' + 'VHBVLB'
049100030620     C                   ELSE
049200030620     C                   MOVEL     WI07          VABVLB
049300030620     C                   ENDIF
049400030620     C                   IF        WD07 = *blanks
049500030620     C                   EVAL      WD07 = *zeros
049600030620     C                   ENDIF
049700030620     C                   TESTN                   WD07                 30
049800030620     C  N30              TESTN                   WD07                   30
049900030620     C  N30              TESTN                   WD07                     30
050000030620     C                   MOVE      WD07          W001A             1
050100030620     C     *IN30         IFEQ      *OFF
050200030620     C     W001A         ORLT      '0'
050300030620     C     W001A         ANDNE     *BLANK
050400030620     C                   SETON                                        32
050500030620     C                   MOVE      *ZERO         VABVLB
050600030620     C                   eval      vinmsg = %trimr(vinmsg)
050700030620     C                             + ' ' + 'VHBVLB'
050800030620     C                   ELSE
050900030620     C                   MOVE      WD07          VABVLB
051000030620     C                   ENDIF
051100030620     C*
051200030620     C                   MOVE      *ZERO         VABQFT
051300030620     C                   MOVE      VHBQFT        WASC15
051400030620     C                   IF        WI15 = *blanks
051500030620     C                   EVAL      WI15 = *zeros
051600030620     C                   ENDIF
051700030620     C                   TESTN                   WI15                 30
051800030620     C  N30              TESTN                   WI15                   30
051900030620     C  N30              TESTN                   WI15                     30
052000030620     C                   MOVE      WI15          W001A             1
052100030620     C     *IN30         IFEQ      *OFF
052200030620     C     W001A         ORLT      '0'
052300030620     C     W001A         ANDNE     *BLANK
052400030620     C                   SETON                                        32
052500030620     C                   MOVE      *ZERO         VABQFT
052600030620     C                   eval      vinmsg = %trimr(vinmsg)
052700030620     C                             + ' ' + 'VHBQFT'
052800030620     C                   ELSE
052900030620     C                   MOVEL     WI15          VABQFT
053000030620     C                   ENDIF
053100030620     C                   IF        WD15 = *blanks
053200030620     C                   EVAL      WD15 = *zeros
053300030620     C                   ENDIF
053400030620     C                   TESTN                   WD15                 30
053500030620     C  N30              TESTN                   WD15                   30
053600030620     C  N30              TESTN                   WD15                     30
053700030620     C                   MOVE      WD15          W001A             1
053800030620     C     *IN30         IFEQ      *OFF
053900030620     C     W001A         ORLT      '0'
054000030620     C     W001A         ANDNE     *BLANK
054100030620     C                   SETON                                        32
054200030620     C                   MOVE      *ZERO         VABQFT
054300030620     C                   eval      vinmsg = %trimr(vinmsg)
054400030620     C                             + ' ' + 'VHBQFT'
054500030620     C                   ELSE
054600030620     C                   MOVE      WD15          VABQFT
054700030620     C                   ENDIF
054800030620     C*
054900030620     C                   MOVE      *ZERO         VABCAS
055000030620     C                   MOVE      VHBCAS        WASC15
055100030620     C                   IF        WI15 = *blanks
055200030620     C                   EVAL      WI15 = *zeros
055300030620     C                   ENDIF
055400030620     C                   TESTN                   WI15                 30
055500030620     C  N30              TESTN                   WI15                   30
055600030620     C  N30              TESTN                   WI15                     30
055700030620     C                   MOVE      WI15          W001A             1
055800030620     C     *IN30         IFEQ      *OFF
055900030620     C     W001A         ORLT      '0'
056000030620     C     W001A         ANDNE     *BLANK
056100030620     C                   SETON                                        32
056200030620     C                   MOVE      *ZERO         VABCAS
056300030620     C                   eval      vinmsg = %trimr(vinmsg)
056400030620     C                             + ' ' + 'VHBCAS'
056500030620     C                   ELSE
056600030620     C                   MOVEL     WI15          VABCAS
056700030620     C                   ENDIF
056800030620     C                   IF        WD15 = *blanks
056900030620     C                   EVAL      WD15 = *zeros
057000030620     C                   ENDIF
057100030620     C                   TESTN                   WD15                 30
057200030620     C  N30              TESTN                   WD15                   30
057300030620     C  N30              TESTN                   WD15                     30
057400030620     C                   MOVE      WD15          W001A             1
057500030620     C     *IN30         IFEQ      *OFF
057600030620     C     W001A         ORLT      '0'
057700030620     C     W001A         ANDNE     *BLANK
057800030620     C                   SETON                                        32
057900030620     C                   MOVE      *ZERO         VABCAS
058000030620     C                   eval      vinmsg = %trimr(vinmsg)
058100030620     C                             + ' ' + 'VHBCAS'
058200030620     C                   ELSE
058300030620     C                   MOVE      WD15          VABCAS
058400030620     C                   ENDIF
058500030620     C*
058600030620     C                   TESTN                   VHBRMN               30
058700030620     C  N30              TESTN                   VHBRMN                 30
058800030620     C  N30              TESTN                   VHBRMN                   30
058900030620     C                   MOVE      VHBRMN        W001A             1
059000030620     C     *IN30         IFEQ      *OFF
059100030620     C     W001A         ORLT      '0'
059200030620     C                   SETON                                        32
059300030620     C                   MOVE      *ZERO         VABRMN
059400030620     C                   eval      vinmsg = %trimr(vinmsg)
059500030620     C                             + ' ' + 'VHBRMN'
059600030620     C                   ELSE
059700030620     C                   MOVE      VHBRMN        VABRMN
059800030620     C                   MOVE      VHBRMN        VABNSP
059900030620     C                   ENDIF
060000030620     C*
060100030620     C* Considerazioni particolari x VABNCD
060200030623     C                   IF        VHBNCD<>*blanks
060300030623     C                   MOVEL     '1'           wFlgDkC           1
060400030620     C                   EVAL      PiStr=%subst(VHBNCD:3:5)
060500030620     C                   EXSR      CHKNUM
060600030620     C                   IF        PiInt=*on
060700030620     C                   Z-ADD     PiVal         wNCD              5 0
060800030620     C                   ELSE
060900030620     C                   Z-ADD     *zeros        wNCD
061000030620     C                   SETON                                        32
061100030620     C                   EVAL      vinmsg = %trimr(vinmsg)
061200030620     C                             + ' ' + 'VABNCD/VABNCA'
061300030620     C                   ENDIF
061400030623     C                   ELSE
061500030623     C                   MOVEL     '0'           wFlgDkC
061600030623     C                   ENDIF
061700030620     C*
061800030620     C                   TESTN                   VHBZNC               30
061900030620     C  N30              TESTN                   VHBZNC                 30
062000030620     C  N30              TESTN                   VHBZNC                   30
062100030620     C                   MOVE      VHBZNC        W001A             1
062200030620     C     *IN30         IFEQ      *OFF
062300030620     C     W001A         ORLT      '0'
062400030620     C     W001A         ANDNE     *BLANK
062500030620     C                   SETON                                        32
062600030620     C                   MOVE      *ZERO         VABZNC
062700030620     C                   eval      vinmsg = %trimr(vinmsg)
062800030620     C                             + ' ' + 'VHBZNC'
062900030620     C                   ELSE
063000030620     C                   MOVE      VHBZNC        VABZNC
063100030620     C                   ENDIF
063200030620     C*
063300030620     C                   TESTN                   VHBDCR               30
063400030620     C  N30              TESTN                   VHBDCR                 30
063500030620     C  N30              TESTN                   VHBDCR                   30
063600030620     C                   MOVE      VHBDCR        W001A             1
063700030620     C     *IN30         IFEQ      *OFF
063800030620     C     W001A         ORLT      '0'
063900030620     C     W001A         ANDNE     *BLANK
064000030620     C                   SETON                                        32
064100030620     C                   MOVE      *ZERO         VABDCR
064200030620     C                   eval      vinmsg = %trimr(vinmsg)
064300030620     C                             + ' ' + 'VHBDCR'
064400030620     C                   ELSE
064500030620     C                   MOVE      VHBDCR        VABDCR
064600030620     C                   ENDIF
064700030620     C*
064800030620     C                   TESTN                   VHBHCR               30
064900030620     C  N30              TESTN                   VHBHCR                 30
065000030620     C  N30              TESTN                   VHBHCR                   30
065100030620     C                   MOVE      VHBHCR        W001A             1
065200030620     C     *IN30         IFEQ      *OFF
065300030620     C     W001A         ORLT      '0'
065400030620     C     W001A         ANDNE     *BLANK
065500030620     C                   SETON                                        32
065600030620     C                   MOVE      *ZERO         VABHCR
065700030620     C                   eval      vinmsg = %trimr(vinmsg)
065800030620     C                             + ' ' + 'VHBHCR'
065900030620     C                   ELSE
066000030620     C                   MOVE      VHBHCR        VABHCR
066100030620     C                   ENDIF
066200030620     C*
066300030620     C                   MOVE      *ZERO         VABVMD
066400030620     C                   MOVE      VHBVMD        WASC15
066500030620     C                   IF        WI15 = *blanks
066600030620     C                   EVAL      WI15 = *zeros
066700030620     C                   ENDIF
066800030620     C                   TESTN                   WI15                 30
066900030620     C  N30              TESTN                   WI15                   30
067000030620     C  N30              TESTN                   WI15                     30
067100030620     C                   MOVE      WI15          W001A             1
067200030620     C     *IN30         IFEQ      *OFF
067300030620     C     W001A         ORLT      '0'
067400030620     C     W001A         ANDNE     *BLANK
067500030620     C                   SETON                                        32
067600030620     C                   MOVE      *ZERO         VABVMD
067700030620     C                   eval      vinmsg = %trimr(vinmsg)
067800030620     C                             + ' ' + 'VHBVMD'
067900030620     C                   ELSE
068000030620     C                   MOVEL     WI15          VABVMD
068100030620     C                   ENDIF
068200030620     C                   IF        WD15 = *blanks
068300030620     C                   EVAL      WD15 = *zeros
068400030620     C                   ENDIF
068500030620     C                   TESTN                   WD15                 30
068600030620     C  N30              TESTN                   WD15                   30
068700030620     C  N30              TESTN                   WD15                     30
068800030620     C                   MOVE      WD15          W001A             1
068900030620     C     *IN30         IFEQ      *OFF
069000030620     C     W001A         ORLT      '0'
069100030620     C     W001A         ANDNE     *BLANK
069200030620     C                   SETON                                        32
069300030620     C                   MOVE      *ZERO         VABVMD
069400030620     C                   EVAL      vinmsg = %trimr(vinmsg)
069500030620     C                             + ' ' + 'VHBVMD'
069600030620     C                   ELSE
069700030620     C                   MOVE      WD15          VABVMD
069800030620     C                   ENDIF
069900030620     C*
070000030620     C                   TESTN                   VHBANT               30
070100030620     C  N30              TESTN                   VHBANT                 30
070200030620     C  N30              TESTN                   VHBANT                   30
070300030620     C                   MOVE      VHBANT        W001A             1
070400030620     C     *IN30         IFEQ      *OFF
070500030620     C     W001A         ORLT      '0'
070600030620     C     W001A         ANDNE     *BLANK
070700030620     C                   SETON                                        32
070800030620     C                   MOVE      *ZERO         VABANT
070900030620     C                   EVAL      vinmsg = %trimr(vinmsg)
071000030620     C                             + ' ' + 'VHBANT'
071100030620     C                   ELSE
071200030620     C                   MOVE      VHBANT        VABANT
071300030620     C                   ENDIF
071400030620     C*
071500030620     C* CAMPI ALFABETICI:
071600030620     C                   MOVE      VHBATB        VABATB
071700030620     C                   MOVE      VHBCBO        VABCBO
071800030620     C                   MOVE      VHBRSD        VABRSD
071900030620     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
072000030620     C     '@':'A'       XLATE     VABRSD        VABRSD
072100030620     C* ==
072200030620     C                   MOVE      VHBRD2        VABRD2
072300030620     C                   MOVE      VHBIND        VABIND
072400030620     C                   MOVE      VHBCAD        VABCAD
072500030620     C                   MOVE      VHBLOD        VABLOD
072600030620     C                   MOVE      VHBPRD        VABPRD
072700030620     C                   MOVE      VHBNZD        VABNZD
072800030620     C                   MOVE      VHBGC1        VABGC1
072900030620     C                   MOVE      VHBGC2        VABGC2
073000030620     C                   MOVE      VHBTSP        VABTSP
073100030620     C                   MOVE      VHBVAS        VABVAS
073200030620     C                   MOVE      VHBNAS        VABNAS
073300030620     C                   MOVE      VHBTIC        VABTIC
073400030620     C                   MOVE      VHBVCA        VABVCA
073500030620     C                   MOVE      VHBGCA        VABGCA
073600030620     C                   MOVE      VHBRMA        VABRMA
073700030620     C                   MOVE      VHBXCO        VABXCO
073800030620     C                   MOVE      VHBNOT        VABNOT
073900030620     C                   MOVE      VHBNT2        VABNT2
074000030623     C                   IF        wFlgDkC = '1'
074100030623     C                   EVAL      VABCTM = '7Q'
074200030623     C                   ELSE
074300030620     C                   MOVE      VHBCTM        VABCTM
074400030623     C                   ENDIF
074500030620     C                   MOVE      VHBFFD        VABFFD
074600030620     C                   MOVE      VHBTCR        VABTCR
074700030620     C                   MOVE      VHBCTS        VABCTS
074800030620     C                   MOVE      VHBFTM        VABFTM
074900030620     C                   MOVE      VHBVAD        VABVAD
075000030620     C                   MOVE      VHBGMA        VABGMA
075100030620     C                   MOVE      VHBGGA        VABGGA
075200030620     C                   MOVE      VHBGVA        VABGVA
075300030620     C                   MOVE      VHBTC1        VABTC1
075400030620     C                   MOVE      VHBTC2        VABTC2
075500030620     C                   MOVE      VHBSCL        VABSCL
075600030620     C                   MOVE      VHBRMO        VABRMO
075700030620     C                   MOVE      VHBCMO        VABCMO
075800030620     C                   MOVE      VHBNMO        VABNMO
075900030620     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
076000030620     C* NELLA NAZIONE ORIGINALE
076100030620     C                   MOVE      VHBNMO        WA1D              1
076200030620     C                   MOVEL     VHBNMO        WA1S              1
076300030620     C     WA1D          IFNE      *BLANK
076400030620     C     WA1S          ANDEQ     *BLANK
076500030620     C                   MOVE      *BLANK        VABNMO
076600030620     C                   ENDIF
076700040531     C*
076800040531     C* Gestisco forzatura codice bolla a 1
076900040531     C                   EVAL      VABCBO = '1'
077000010202     C*
077100000801     C* Ebbene...
077200000801     C                   ADD       1             §CTRMO
077300030620     C                   IF        *IN31 = *ON OR *IN32 = *ON
077400000801     C                   ADD       1             §CTRNO
077500000801     C                   EVAL      recko = vindta
077600000801     C                   EXCEPT    rigadett
077700000801     C                   EVAL      vinflg = '2'
077800000801     C                   ELSE
077900010201     C                   ADD       1             §CTROKVB
078000000801     C                   ENDIF
078100000801     C*
078200030620     C                   endif
078300030620     C*
078400000801     C                   ENDSR
078500030226     C*----------------------------------------------------*
078600030226     C*  SCARICAMENTO BUFFER RECORDS VAB
078700030226     C*----------------------------------------------------*
078800030226     C     WRIVAB        BEGSR
078900030310     C*
079000030310     C                   write     FIVAB000
079100030226     C*
079200030226     C                   ENDSR
079300030226     C*----------------------------------------------------*
079400030620     C*  IMPOSTAZIONE CAMPI X DETTAGLIO COLLI DISK "C"
079500030226     C*----------------------------------------------------*
079600030226     C     IMPVAT        BEGSR
079700030226     C*
079800030620     C                   EVAL      VATATB = VABATB
079900030620     C                   EVAL      VATFGS = VABFGS
080000030620     C                   EVAL      VATCCM = VABCCM
080100030620     C                   EVAL      VATLNP = VABLNP
080200030620     C                   EVAL      VATAAS = VABAAS
080300030620     C                   EVAL      VATNRS = VABNRS
080400030620     C                   EVAL      VATNSP = VABNSP
080500030620     C                   EVAL      VATTRC = 'E'
080600040114     C*
080700040114     C* Dall'anno della spedizione reperisco gli ultimi 2 byte che andranno a formare parte del
080800040114     C* barcode
080900040114     C                   MOVE      vabAAS        wAAS              2
081000030620     C*
081100030620     C* Partendo dal campo "Segnacollo DAL" creo tanti VAT quanti sono i colli in testata bolle
081200030620     C                   Z-ADD     *zeros        wVATn             5 0
081300030620     C                   MOVEL     *blanks       wVATa             5
081400030620     C     1             DO        vabNCL        wNCL              5 0
081500030620     C                   EVAL      wVATn = wNCD+wNCL-1
081600030620     C                   MOVE      wVATn         wVATa
081700040114     C***                EVAL      VATNOT=%subst(VABRMA:1:6) + '03' +
081800040114     C                   EVAL      VATNOT=%subst(VABRMA:1:6) + wAAS +
081900030623     C                                    %subst(VHBNCD:2:1) + wVATa
082000030620     C                   EXSR      wrivat                                       => scarico VAT
082100030620     C                   ENDDO
082200030226     C*
082300030226     C                   ENDSR
082400010201     C*----------------------------------------------------*
082500030620     C*  SCRITTURA RECORD DI DETTAGLIO X DISK "C"
082600010201     C*----------------------------------------------------*
082700020305     C     WRIVAT        BEGSR
082800010201     C*
082900021017     C                   write     FIVAT000
083000010201     C*
083100010201     C                   ENDSR
083200030620     C*----------------------------------------------------*
083300030620     C*  CONTROLLO NUMERICITA' CAMPI
083400030620     C*----------------------------------------------------*
083500030620     C     CHKNUM        BEGSR
083600030620     C*
083700030620     C                   call(e)   'ISNUMERIC'
083800030620     C                   PARM                    PiStr            30
083900030620     C                   PARM      '.'           PiDecChr          1
084000030620     C                   PARM      *ZEROS        PiVal            30 9
084100030620     C                   PARM      '0'           PiInt             1
084200030620     C                   PARM      '0'           PiNum             1
084300030620     C                   IF        %error
084400030620     C                   EVAL      PiInt=*off
084500030620     C                   EVAL      PiNum=*off
084600030620     C                   ENDIF
084700030620     C*
084800030620     C                   ENDSR
084900030620     C***
085000030620
085100030620
085200030620
085300010202     C*----------------------------------------------------*
085400021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
085500010202     C*----------------------------------------------------*
085600020305     C     PREVAT        BEGSR
085700010202     C*
085800021017     C* Compongo il nome del membro da dare al FIVATWWR
085900010202     C                   eval      parmbr = vlrhdl
086000010202     C                   movel     'M'           parmbr
086100030506     C                   eval      parccm = '00501798'
086200010202     C                   eval      paropz = '1'
086300010202     C* Effettuo la chiamata al CLLE preposto
086400030620     C                   call(e)   'TITVVTC'
086500010202     C                   parm                    parccm
086600010202     C                   parm                    parmbr
086700010202     C                   parm                    paropz
086800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
086900010202     C                   if        %error
087000010202     C                   movel     '1'           chkcall
087100010202     C                   else
087200010202     C                   movel     '0'           chkcall
087300010202     C                   endif
087400010202     C*
087500010202     C                   ENDSR
087600000801
087700011113
087800011113     C*----------------------------------------------------*
087900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
088000011113     C*----------------------------------------------------*
088100011113     C     CHKIMPDIV     BEGSR
088200011113     C*
088300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
088400011113     C                   Z-ADD     *zeros        wrkDec            9 9
088500011113     C*
088600011113     C* Come prima cosa effettuo considerazioni sulla divisa
088700011113     C                   IF        vabIAS > *zeros
088800011113     C                   IF        vabVAS <> 'EUR'
088900011113     C                   EVAL      vabVAS =  'ITL'
089000011113     C                   ENDIF
089100011113     C                   ENDIF
089200011113     C*
089300011113     C                   IF        vabCAS > *zeros
089400011113     C                   IF        vabVCA <> 'EUR'
089500011113     C                   EVAL      vabVCA =  'ITL'
089600011113     C                   ENDIF
089700011113     C                   ENDIF
089800011113     C*
089900011113     C                   IF        vabVMD > *zeros
090000020305     C                   IF        vabVAD <> 'EUR'
090100011113     C                   EVAL      vabVAD =  'ITL'
090200011113     C                   ENDIF
090300011113     C                   ENDIF
090400011113     C*
090500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
090600011113     C                   Z-ADD     vabIAS        wrkDec
090700011113     C                   IF        wrkDec > *zeros
090800011113     C                   IF        vabVAS = 'ITL'
090900011113     C                   EVAL      vabIAS = *zeros
091000011113     C                   ENDIF
091100011113     C                   ENDIF
091200011113     C*
091300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
091400011113     C                   Z-ADD     vabCAS        wrkDec
091500011113     C                   IF        wrkDec > *zeros
091600011113     C                   IF        vabVCA = 'ITL'
091700011113     C                   EVAL      vabCAS = *zeros
091800011113     C                   ENDIF
091900011113     C                   ENDIF
092000011113     C*
092100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
092200011113     C                   Z-ADD     vabVMD        wrkDec
092300011113     C                   IF        wrkDec > *zeros
092400011113     C                   IF        vabVAD = 'ITL'
092500011113     C                   EVAL      vabVMD = *zeros
092600011113     C                   ENDIF
092700011113     C                   ENDIF
092800011113     C*
092900011113     C                   ENDSR
093000011113     C***
093100021017
093200000801
093300000801
093400990920      /TITLE Invio dei dati al punto operativo.
093500010202     C     invio         BEGSR
093600990920     C*
093700021017     C* 1° invio FIVAT
093800010201     C                   reset                   dscmz
093900021017     C                   move      vlrpoi        cmzdst
094000021017     C                   eval      cmzfld = 'FIVATWWR'
094100010201     C                   eval      cmzmbd = vlrhdl
094200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
094300021009     C***                if        prmfir = *blanks
094400021017     C                   eval      cmzfla = 'FIVAT00F'
094500021017     C                   eval      cmzmba = 'FIVAT00F'
094600021009     C***                else
094700021009     C***                eval      cmzfla = prmfir
094800021009     C***                eval      cmzmba = prmfir
094900021009     C***                endif
095000010201     C                   eval      cmznrr = *zeros
095100020305     C                   move      §ctrokvt      cmznrr
095200021018     C                   eval      cmzlba = vlrfl1
095300010201     C                   call(e)   'TIS711C'
095400010201     C                   parm                    dscmz
095500010201     C                   parm      *blanks       esito
095600010205     C                   if        %error
095700010205     C                             or cmzerr = '1'
095800010205     C                             or esito  = '1'
095900010205     C                   eval      wrkesito = '3'
096000010205     C                   else
096100010201     C*
096200021017     C* 2° invio FIVAB
096300010201     C                   reset                   dscmz
096400021017     C                   move      vlrpoi        cmzdst
096500010201     C                   eval      cmzfld = vlrfou
096600010201     C                   eval      cmzmbd = vlrhdl
096700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
096800021009     C***                if        prmfir = *blanks
096900021017     C                   eval      cmzfla = 'FIVAB00F'
097000021017     C                   eval      cmzmba = 'FIVAB00F'
097100021009     C***                else
097200021009     C***                eval      cmzfla = prmfir
097300021009     C***                eval      cmzmba = prmfir
097400021009     C***                endif
097500010201     C                   eval      cmznrr = *zeros
097600010201     C                   move      §ctrokvb      cmznrr
097700021018     C                   eval      cmzlba = vlrfl1
097800010201     C                   call(e)   'TIS711C'
097900010201     C                   parm                    dscmz
098000010201     C                   parm      *blanks       esito
098100010201     C                   if        %error
098200010201     C                             or cmzerr = '1'
098300010201     C                             or esito  = '1'
098400010201     C                   eval      wrkesito = '3'
098500010201     C                   endif
098600010205     C                   endif
098700990920     C*
098800000613     C                   ENDSR
098900000613     C***
099000070411
099100070411     C     *pssr         BEGSR
099200070411     C*
099300070411     C                   if        %open(tivin00r)
099400070411     C                   close     tivin00r
099500070411     C                   endif
099600070411     C                   if        %open(fivabwwr)
099700070411     C                   close     fivabwwr
099800070411     C                   endif
099900070411     C                   if        %open(fivatwwr)
100000070411     C                   close     fivatwwr
100100070411     C                   endif
100200070411     C*
100300070411     C* Effettuo la chiamata al CLLE preposto
100400070411     C                   call(e)   'TITVVTC'
100500070411     C                   parm                    parccm
100600070411     C                   parm                    parmbr
100700070411     C                   parm      '2'           paropz
100800070411     C*
100900070411     C                   eval      wrkesito = '2'
101000070411     C*
101100070411     C                   seton                                        LR
101200070411     C*
101300070411     C                   ENDSR     '*CANCL'
101400070411     C***
101500070411
101600990910
101700000613     C     *inzsr        BEGSR
101800990910     C*
101900990910     C     *entry        plist
102000990920     C                   parm                    tivlrds
102100990921     C                   parm      wrkesito      esito
102200000724     C                   parm                    prmlit
102300000710     C                   parm                    prmfir
102400000613     C*
102500000830     C* CALCOLA LA DATA CORRENTE
102600000830     C                   time                    wn14             14 0
102700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
102800000830     C                   z-add     wn8           g08dat
102900000830     C                   z-add     *zeros        g08inv
103000000830     C                   movel     '0'           g08err
103100000830     C                   call      'XSRDA8'
103200000830     C                   parm                    wlbda8
103300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
103400000830     C*
103500000613     C                   ENDSR
103600000613     C***
103700990908
103800030620     Otitv58p   E            riepilogo         2
103900990915     O                                              'Upload via Internet'
104000990915     O                                           +1 'Traduzione TIVIN00R -'
104100021017     O                                           +1 'FIVABWWR/FIVATWWR'
104200010201     O                                           +1 'Report testate bolle'
104300990915     O          E            riepilogo   2
104400990915     O                       wrkdata
104500990915     O                       wrkora              +1
104600990915     O                       procname            +1
104700990915     O          E            riepilogo   2
104800990915     O                                              'Cliente..................:'
104900990915     O                       VABCCM        z     +1
105000990915     O          E            riepilogo   2
105100990920     O                                              'Riferimento Strategi.....:'
105200990920     O                       vlrhdl              +1
105300990915     O          E            riepilogo   2
105400990915     O                                              'Giusti...................:'
105500010201     O                       §CTROKVB      2   +  1
105600990915     O          E            riepilogo   2
105700990915     O                                              'Sbagliati e corretti.....:'
105800971022     O                       §CTRMO        2   +  1
105900990915     O          E            riepilogo   2
106000990915     O                                              'Sbagliati e scartati.....:'
106100971022     O                       §CTRNO        2   +  1
106200000613
106300030620     Otitv58ps  E            testdett          2
106400000613     O                                              'Upload via Internet'
106500000613     O                                           +1 'Traduzione TIVIN00R -'
106600021017     O                                           +1 'FIVABWWR/FIVATWWR'
106700010201     O                                           +1 'Report testate bolle'
106800000616     O          E            testdett    3
106900000613     O                                           +2 'N° rec'
107000000613     O                                           +3 'Anteprima contenuto'
107100000616     O          E            rigadett    2
107200000613     O                       rrnum               +2
107300000621     O                       recko               +3
107400000616     O          E            findett     2
107500000613     O                       wrkdata
107600000613     O                       wrkora              +1
107700000613     O                       procname            +1
107800000616     O          E            findett     2
107900000613     O                                              'Cliente..................:'
108000000613     O                       VABCCM        z     +1
108100000616     O          E            findett     2
108200000613     O                                              'Riferimento Strategi.....:'
108300000613     O                       vlrhdl              +1
108400000616     O          E            findett     2
108500000613     O                                              'Giusti...................:'
108600010201     O                       §CTROKVB      2   +  1
108700000616     O          E            findett     2
108800000613     O                                              'Sbagliati e corretti.....:'
108900000613     O                       §CTRMO        2   +  1
109000000616     O          E            findett     2
109100000613     O                                              'Sbagliati e scartati.....:'
109200000613     O                       §CTRNO        2   +  1
109300000512** CMD - COMANDI CL
109400030620OVRPRTF FILE(TITV58P)  TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
109500030620OVRPRTF FILE(TITV58PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
109600030620DLTOVR FILE(TITV58P TITV58PS) LVL(*)
109700000512
109800000512
