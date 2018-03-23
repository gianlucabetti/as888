000100021014      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021014     FFIVABwwr  O    E             DISK    usropn
000700021014     FFIVATwwr  O    E             DISK    usropn
000800021015     Ftitv25p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100021015     Ftitv25ps  O    f  198        PRINTER usropn
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
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600021016     D fnvabaar      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600020305     D depspe          s             14    INZ(*blanks)
003700010213     D depdat          s              8  0 INZ(*zeros)
003800020305     D curspe          s             14    INZ(*blanks)
003900010202     D parccm          s              8    INZ(*blanks)
004000010202     D parmbr          s             10    INZ(*blanks)
004100010202     D paropz          s              1    INZ(*blanks)
004200010202     D chkcall         s              1    INZ(*blanks)
004300000830
004400000830     D*------------------
004500000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004600000830     D*------------------
004700000830     D WLBDA8          DS                  INZ
004800000830     D  G08DAT                 1      8  0
004900000830     D  G08INV                 9     16  0
005000000830     D  G08ERR                17     17
005100000830     D  G08TGI                18     22  0
005200000830     D*
005300021015     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
005400021015     D* 10+3
005500021015     D                 DS
005600021015     D  WS15                   1      1
005700021015     D  WI15                   2     11
005800021015     D  WV15                  12     12
005900021015     D  WD15                  13     15
006000021015     D  WASC15                 1     15
006100021015     D*  6+1
006200021015     D                 DS
006300021015     D  WS09                   1      1
006400021015     D  WI09                   2      7
006500021015     D  WV09                   8      8
006600021015     D  WD09                   9      9
006700021015     D  WASC09                 1      9
006800021015     D*  2+3
006900021015     D                 DS
007000021015     D  WS07                   1      1
007100021015     D  WI07                   2      3
007200021015     D  WV07                   4      4
007300021015     D  WD07                   5      7
007400021015     D  WASC07                 1      7
007500990908
007600010201
007700010201
007800990915     C                   time                    wrkdata
007900990915     C                   time                    wrkora
008000000913     C                   reset                   rrnum
008100990921     C                   reset                   esito
008200990921     C                   reset                   wrkesito
008300000613     C*
008400021014     C                   EXSR      OPEINI
008500021014     C                   EXSR      RWALLISON                                    LETT/SCR. VAB
008600000613     C*
008700010202     C* Effettuo la chiamata al CLLE preposto
008800050128     C                   call(e)   'TITVVTC'
008900010202     C                   parm                    parccm
009000010202     C                   parm                    parmbr
009100010202     C                   parm      '2'           paropz
009200000616     C*
009300000801     C
009400010201     C                   seton                                        LR
009500000613
009600000613
009700000613     C*--------------------------------------------------------
009800000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
009900000613     C*--------------------------------------------------------
010000000621     C     STPR          BEGSR
010100000613     C*
010200021015     C                   if        not %open(titv25p)
010300021015     C                   open      titv25p
010400990915     C                   endif
010500990915     C*
010600990915     C                   except    riepilogo
010700990915     C*
010800021015     C                   if        %open(titv25p)
010900021015     C                   close     titv25p
011000990914     C                   endif
011100990910     C*
011200000613     C                   ENDSR
011300000613     C***
011400990908
011500000801
011600910830     C*--------------------------------------------------------
011700021014     C* RWALLISON LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR *
011800910830     C*--------------------------------------------------------
011900021014     C     RWALLISON     BEGSR
012000990910     C*
012100990914     C                   if        not %open(tivin00r)
012200990908     C                   open      tivin00r
012300990914     C                   endif
012400021014     C                   if        not %open(fivabwwr)
012500021014     C                   open      fivabwwr
012600990914     C                   endif
012700021014     C*
012800021014     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
012900020305     C                   exsr      prevat
013000010201     C*
013100010202     C                   if        chkcall = '0'
013200010202     C*
013300021014     C                   if        not %open(fivatwwr)
013400021014     C                   open      fivatwwr
013500010201     C                   endif
013600990910     C*
013700010201     C                   clear                   §CTROKVB          5 0
013800020305     C                   clear                   §CTROKVT          5 0
013900000801     C                   clear                   §CTRMO            5 0
014000000801     C                   clear                   §CTRNO            5 0
014100990910     C*
014200921023     C                   DO        *HIVAL
014300990913     C*
014400990915     C                   READ      tivin00r                               70
014500000905     C                   if        vindta > *blanks
014600000613     C                   add       1             rrnum
014700021014     C* ...elimino eventuali segni
014800021014     C     '+':' '       xlate     vindta        vindta
014900000801     C*
015000000801     C                   if        *in70 = *off
015100000801     C                             and
015200000801     C                             (vinflg = *blanks
015300000801     C                              or vinflg = '0'
015400000801     C                              or vinflg = '2')
015500000801     C*
015600000801     C                   clear                   vinmsg
015700021014     C*
015800020305     C                   exsr      impvab
015900021014     C*
016000021014     C                   endif
016100000905     C                   else
016200000905     C                   eval      vinflg = '1'
016300000905     C                   endif
016400000905     C*
016500000905     C  N70              update    tivin000
016600000905     C*
016700991022     C  N70              ENDdo
016800010202     C*
016900010202     C                   endif
017000990910
017100021014     C*
017200021014     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017300021014     C                   if        cntNonEl = *zeros or
017400021014     C                             flgMulti = '0'
017500021014     C* Se non ci sono record con errori ...
017600021014     C                   if        §ctrno = 0 and
017700021014     C                             §ctrmo = 0 and
017800021014     C                             flgStato <> '2'
017900021014     C* ... restituisco esito OK.
018000021014     C                   eval      wrkesito = '0'
018100021014     C                   else
018200021014     C                   if        §ctrokvb > 0
018300021014     C                   eval      wrkesito = '1'
018400021014     C                   else
018500021014     C                   if        flgOk = '0'
018600021014     C                   eval      wrkesito = '2'
018700021014     C                   else
018800021014     C                   eval      wrkesito = '6'
018900021014     C                   endif
019000021014     C                   endif
019100021014     C                   endif
019200021014     C                   else
019300021014     C                   eval      wrkesito = '9'
019400021014     C                   endif
019500990910     C*
019600990914     C                   if        %open(tivin00r)
019700990908     C                   close     tivin00r
019800990914     C                   endif
019900021014     C                   if        %open(fivabwwr)
020000021014     C                   close     fivabwwr
020100990914     C                   endif
020200021014     C                   if        %open(fivatwwr)
020300021014     C                   close     fivatwwr
020400010201     C                   endif
020500990910     C*
020600010201     C                   if        §ctrokvb > 0
020700021014     C                             and invfil <> *zeros
020800010202     C                   exsr      invio
020900990920     C                   endif
021000021015     C*
021100021015     C                   if        flgGiro = '1'
021200021015     C                   exsr      endela
021300021015     C                   endif
021400990920     C*
021500910830     C                   ENDSR
021600000613     C***
021700010305
021800010305     C*----------------------------------------------------*
021900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
022000010305     C*----------------------------------------------------*
022100020305     C     WRIVAB        BEGSR
022200010305     C*
022300021014     C                   write     fivab000                                     => scarico il VAB
022400010305     C*
022500010305     C                   ENDSR
022600990920
022700000801     C*----------------------------------------------------*
022800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
022900000801     C*----------------------------------------------------*
023000000801     C     DEFCAM        BEGSR
023100000801     C*
023200020619     C* Imposto i valori di default...
023300020801     C                   MOVEL     '1'           VABCBO
023400020305     C                   MOVEL     '7Q'          VABCTM
023500020305     C                   MOVEL     'E'           VATTRC
023600020619     C* ... e poi verifico se sono stati passati come parametri
023700020619     C                   IF        vlrppt > *blanks
023800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023900020619     C                   EXSR      CHKNUM
024000020619     C                   IF        PiInt=*on
024100020619     C                   Z-ADD     PiVal         VABCCM
024200020619     C                   Z-ADD     PiVal         VATCCM
024300020619     C                   ENDIF
024400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024500020619     C                   EXSR      CHKNUM
024600020619     C                   IF        PiInt=*on
024700020619     C                   Z-ADD     PiVal         VABLNP
024800020619     C                   Z-ADD     PiVal         VATLNP
024900020619     C                   ENDIF
025000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025100020619     C                   EXSR      CHKNUM
025200020619     C                   IF        PiInt=*on
025300020619     C                   Z-ADD     PiVal         VABCTR
025400020619     C                   ENDIF
025500020619     C                   ENDIF
025600000801     C*
025700000801     C                   ENDSR
025800021015
025900021015
026000021015     C*--------------------------------------------------------
026100021015     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
026200021015     C*--------------------------------------------------------
026300021015     C     PREELA        BEGSR
026400021015     C*
026500021015     C* SE OCCORRE SPEDIRE IN FILIALE
026600021015     C                   if        invfil <> *zeros and
026700021015     C                             flgGiro = '0'
026800021015     C*
026900021015     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
027000021015     C                   eval      flgGiro = '1'
027100021015     C*
027200021015     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
027300021015     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
027400021015     C     invfil        chain     azorg01l
027500021015     C                   if        %found
027600021015     C                   movel(p)  CMD(1)        depcmd
027700021015     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
027800021015     C*
027900021015     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
028000021015     C                   Z-ADD     150           LENGH            15 5
028100021015     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
028200021015     C                   PARM                    depcmd
028300021015     C                   PARM                    LENGH
028400021015     C*
028500021015     C                   endif
028600021015     C*
028700021015     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
028800021015     C                   movel(p)  CMD(2)        depcmd
028900021015     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
029000021015     C*
029100021015     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
029200021015     C                   Z-ADD     150           LENGH            15 5
029300021015     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
029400021015     C                   PARM                    depcmd
029500021015     C                   PARM                    LENGH
029600021015     C*
029700021015     C                   if        not %open(titv25ps)
029800021015     C                   open      titv25ps
029900021015     C                   except    testdett
030000021015     C                   endif
030100021015     C*
030200021015     C                   endif
030300021015     C*
030400021015     C                   ENDSR
030500021015     C***
030600021015
030700021015
030800021015
030900021015
031000021015     C*--------------------------------------------------------
031100021015     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
031200021015     C*--------------------------------------------------------
031300021015     C     ENDELA        BEGSR
031400021015     C*
031500021015     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
031600021015     C*
031700021015     C                   if        %open(titv25ps)
031800021015     C                   except    findett
031900021015     C                   close     titv25ps
032000021015     C                   endif
032100021015     C*
032200021015     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
032300021015     C                   Z-ADD     150           LENGH            15 5
032400021015     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
032500021015     C                   PARM                    CMD(3)
032600021015     C                   PARM                    LENGH
032700021015     C*
032800021015     C                   ENDSR
032900021015     C***
033000021015
033100021015
033200021015
033300000801     C*----------------------------------------------------*
033400021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033500000801     C*----------------------------------------------------*
033600010201     C     IMPVAB        BEGSR
033700021014     C*
033800021014     C                   CLEAR                   FIVAB000
033900021018     C                   CLEAR                   FIVAT000
034000000801     C*
034100020305     C                   EXSR      DEFCAM
034200010305     C*
034300021016     C                   eval      fnvabaar = vindta
034400021014     C*
034500021014     C                   testb     x'3F'         vhbatb                   02
034600021014     C                   if        not *in02
034700021014     C*
034800021014     C                   SETOFF                                       303132
034900021014     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
035000021014     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
035100021014     C*    OPPURE TUTTI BLANKS
035200021014     C*
035300021014     C                   TESTN                   VHBLNP               30
035400021014     C  N30              TESTN                   VHBLNP                 30
035500021014     C  N30              TESTN                   VHBLNP                   30
035600021014     C                   MOVE      VHBLNP        W001A             1
035700021014     C     *IN30         IFEQ      *OFF
035800021014     C     W001A         ORLT      '0'
035900021014     C                   SETON                                        31
036000021015     C                   EVAL      vinmsg = %trimr(vinmsg)
036100021014     C                             + ' ' + 'VHBLNP'
036200021014     C                   ELSE
036300021014     C                   MOVE      VHBLNP        VABLNP
036400021014     C     VABLNP        IFLE      *ZEROS
036500021014     C                   SETON                                        31
036600021015     C                   EVAL      vinmsg = %trimr(vinmsg)
036700021014     C                             + ' ' + 'VHBLNP'
036800021014     C                   ENDIF
036900021014     C                   ENDIF
037000021014     C*
037100021014     C* Effettuo considerazioni x elaborazioni "multi-filiale"
037200021014     C                   eval      depfil = VABLNP
037300021014     C                   exsr      repfil
037400021014     C                   if        depfil = invfil
037500021016     C                   if        vlrpoi = 999
037600021016     C                   MOVE(P)   invfil        VABFGS
037700021016     C                   else
037800021016     C                   MOVE(P)   vlrpoi        VABFGS
037900021016     C                   endif
038000021014     C*
038100021014     C                   exsr      PREELA
038200021014     C*
038300021014     C                   TESTN                   VHBCCM               30
038400021014     C  N30              TESTN                   VHBCCM                 30
038500021014     C  N30              TESTN                   VHBCCM                   30
038600021014     C                   MOVE      VHBCCM        W001A             1
038700021014     C     *IN30         IFEQ      *OFF
038800021014     C     W001A         ORLT      '0'
038900021014     C                   SETON                                        31
039000021015     C                   EVAL      vinmsg = %trimr(vinmsg)
039100021014     C                             + ' ' + 'VHBCCM'
039200021014     C                   ELSE
039300021014     C                   MOVE      VHBCCM        VABCCM
039400021014     C     VABCCM        IFLE      *ZEROS
039500021014     C                   SETON                                        31
039600021015     C                   EVAL      vinmsg = %trimr(vinmsg)
039700021014     C                             + ' ' + 'VHBCCM'
039800021014     C                   ENDIF
039900021014     C                   ENDIF
040000021014     C*
040100021014     C                   TESTN                   VHBAAS               30
040200021014     C  N30              TESTN                   VHBAAS                 30
040300021014     C  N30              TESTN                   VHBAAS                   30
040400021014     C                   MOVE      VHBAAS        W001A             1
040500021014     C     *IN30         IFEQ      *OFF
040600021014     C     W001A         ORLT      '0'
040700021014     C     W001A         ANDNE     *BLANK
040800021014     C                   SETON                                        32
040900021014     C                   MOVEL     datcor        VABAAS
041000021015     C                   EVAL      vinmsg = %trimr(vinmsg)
041100021014     C                             + ' ' + 'VHBAAS'
041200021014     C                   ELSE
041300021014     C                   MOVE      VHBAAS        VABAAS
041400021014     C                   ENDIF
041500021014     C*
041600021014     C                   TESTN                   VHBMGS               30
041700021014     C  N30              TESTN                   VHBMGS                 30
041800021014     C  N30              TESTN                   VHBMGS                   30
041900021014     C                   MOVE      VHBMGS        W001A             1
042000021014     C     *IN30         IFEQ      *OFF
042100021014     C     W001A         ORLT      '0'
042200021014     C     W001A         ANDNE     *BLANK
042300021014     C                   SETON                                        32
042400021014     C                   MOVE      '0101'        VABMGS
042500021015     C                   EVAL      vinmsg = %trimr(vinmsg)
042600021014     C                             + ' ' + 'VHBMGS'
042700021014     C                   ELSE
042800021014     C                   MOVE      VHBMGS        VABMGS
042900021014     C                   ENDIF
043000021014     C*
043100021021     C*                  TESTN                   VHBNRS               30
043200021021     C* N30              TESTN                   VHBNRS                 30
043300021021     C* N30              TESTN                   VHBNRS                   30
043400021021     C*                  MOVE      VHBNRS        W001A             1
043500021021     C*    *IN30         IFEQ      *OFF
043600021021     C*    W001A         ORLT      '0'
043700021021     C*    W001A         ANDNE     *BLANK
043800021021     C*                  SETON                                        32
043900021021     C*                  MOVE      *ZERO         VABNRS
044000021021     C*                  EVAL      vinmsg = %trimr(vinmsg)
044100021021     C*                            + ' ' + 'VHBNRS'
044200021021     C*                  ELSE
044300021021     C*                  MOVE      VHBNRS        VABNRS
044400021021     C*                  ENDIF
044500021014     C*
044600021014     C                   TESTN                   VHBNSP               30
044700021014     C  N30              TESTN                   VHBNSP                 30
044800021014     C  N30              TESTN                   VHBNSP                   30
044900021014     C                   MOVE      VHBNSP        W001A             1
045000021014     C     *IN30         IFEQ      *OFF
045100021014     C     W001A         ORLT      '0'
045200021014     C     W001A         ANDNE     *BLANK
045300021014     C                   SETON                                        32
045400021014     C                   MOVE      *ZERO         VABNSP
045500021015     C                   EVAL      vinmsg = %trimr(vinmsg)
045600021014     C                             + ' ' + 'VHBNSP'
045700021014     C                   ELSE
045800021014     C                   MOVE      VHBNSP        VABNSP
045900021014     C                   ENDIF
046000021014     C*
046100021014     C                   TESTN                   VHBLNA               30
046200021014     C  N30              TESTN                   VHBLNA                 30
046300021014     C  N30              TESTN                   VHBLNA                   30
046400021014     C                   MOVE      VHBLNA        W001A             1
046500021014     C     *IN30         IFEQ      *OFF
046600021014     C     W001A         ORLT      '0'
046700021014     C     W001A         ANDNE     *BLANK
046800021014     C                   SETON                                        32
046900021014     C                   MOVE      *ZERO         VABLNA
047000021015     C                   EVAL      vinmsg = %trimr(vinmsg)
047100021014     C                             + ' ' + 'VHBLNA'
047200021014     C                   ELSE
047300021014     C                   MOVE      VHBLNA        VABLNA
047400021014     C                   ENDIF
047500021014     C*
047600021014     C                   TESTN                   VHBCTR               30
047700021014     C  N30              TESTN                   VHBCTR                 30
047800021014     C  N30              TESTN                   VHBCTR                   30
047900021014     C                   MOVE      VHBCTR        W001A             1
048000021014     C     *IN30         IFEQ      *OFF
048100021014     C     W001A         ORLT      '0'
048200021014     C     W001A         ANDNE     *BLANK
048300021014     C                   SETON                                        32
048400021014     C                   MOVE      *ZERO         VABCTR
048500021015     C                   EVAL      vinmsg = %trimr(vinmsg)
048600021014     C                             + ' ' + 'VHBCTR'
048700021014     C                   ELSE
048800021014     C                   MOVE      VHBCTR        VABCTR
048900021014     C                   ENDIF
049000021014     C*
049100021014     C                   MOVE      *ZERO         VABIAS
049200021014     C                   MOVE      VHBIAS        WASC15
049300021014     C                   TESTN                   WI15                 30
049400021014     C  N30              TESTN                   WI15                   30
049500021014     C  N30              TESTN                   WI15                     30
049600021014     C                   MOVE      WI15          W001A             1
049700021014     C     *IN30         IFEQ      *OFF
049800021014     C     W001A         ORLT      '0'
049900021014     C     W001A         ANDNE     *BLANK
050000021014     C                   SETON                                        32
050100021014     C                   MOVE      *ZERO         VABIAS
050200021015     C                   EVAL      vinmsg = %trimr(vinmsg)
050300021014     C                             + ' ' + 'VHBIAS'
050400021014     C                   ELSE
050500021014     C                   MOVEL     WI15          VABIAS
050600021014     C                   ENDIF
050700021014     C                   TESTN                   WD15                 30
050800021014     C  N30              TESTN                   WD15                   30
050900021014     C  N30              TESTN                   WD15                     30
051000021014     C                   MOVE      WD15          W001A             1
051100021014     C     *IN30         IFEQ      *OFF
051200021014     C     W001A         ORLT      '0'
051300021014     C     W001A         ANDNE     *BLANK
051400021014     C                   SETON                                        32
051500021014     C                   MOVE      *ZERO         VABIAS
051600021015     C                   EVAL      vinmsg = %trimr(vinmsg)
051700021014     C                             + ' ' + 'VHBIAS'
051800021014     C                   ELSE
051900021014     C                   MOVE      WD15          VABIAS
052000021014     C                   ENDIF
052100021014     C*
052200021014     C                   TESTN                   VHBNCL               30
052300021014     C  N30              TESTN                   VHBNCL                 30
052400021014     C  N30              TESTN                   VHBNCL                   30
052500021014     C                   MOVE      VHBNCL        W001A             1
052600021014     C     *IN30         IFEQ      *OFF
052700021014     C     W001A         ORLT      '0'
052800021014     C                   SETON                                        32
052900021014     C                   MOVE      *ZERO         VABNCL
053000021015     C                   EVAL      vinmsg = %trimr(vinmsg)
053100021014     C                             + ' ' + 'VHBNCL'
053200021014     C                   ELSE
053300021014     C                   MOVE      VHBNCL        VABNCL
053400021014     C                   ENDIF
053500021014     C*
053600021014     C                   MOVE      *ZERO         VABPKB
053700021014     C                   MOVE      VHBPKB        WASC09
053800021014     C                   IF        WI09 = *blanks
053900021014     C                   EVAL      WI09 = *zeros
054000021014     C                   ENDIF
054100021014     C                   TESTN                   WI09                 30
054200021014     C  N30              TESTN                   WI09                   30
054300021014     C  N30              TESTN                   WI09                     30
054400021014     C                   MOVE      WI09          W001A             1
054500021014     C     *IN30         IFEQ      *OFF
054600021014     C     W001A         ORLT      '0'
054700021014     C                   SETON                                        32
054800021014     C                   MOVE      *ZERO         VABPKB
054900021015     C                   EVAL      vinmsg = %trimr(vinmsg)
055000021014     C                             + ' ' + 'VHBPKB'
055100021014     C                   ELSE
055200021014     C                   MOVEL     WI09          VABPKB
055300021014     C                   ENDIF
055400021014     C                   IF        WD09 = *blanks
055500021014     C                   EVAL      WD09 = *zeros
055600021014     C                   ENDIF
055700021014     C                   TESTN                   WD09                 30
055800021014     C  N30              TESTN                   WD09                     30
055900021014     C                   MOVE      WD09          W001A             1
056000021014     C     *IN30         IFEQ      *OFF
056100021014     C     W001A         ORLT      '0'
056200021014     C     W001A         ANDNE     *BLANK
056300021014     C                   SETON                                        32
056400021014     C                   MOVE      *ZERO         VABPKB
056500021015     C                   EVAL      vinmsg = %trimr(vinmsg)
056600021014     C                             + ' ' + 'VHBPKB'
056700021014     C                   ELSE
056800021014     C                   MOVE      WD09          VABPKB
056900021014     C                   ENDIF
057000021014     C*
057100021014     C                   MOVE      *ZERO         VABVLB
057200021014     C                   MOVE      VHBVLB        WASC07
057300021014     C                   IF        WI07 = *blanks
057400021014     C                   EVAL      WI07 = *zeros
057500021014     C                   ENDIF
057600021014     C                   TESTN                   WI07                 30
057700021014     C  N30              TESTN                   WI07                   30
057800021014     C  N30              TESTN                   WI07                     30
057900021014     C                   MOVE      WI07          W001A             1
058000021014     C     *IN30         IFEQ      *OFF
058100021014     C     W001A         ORLT      '0'
058200021014     C     W001A         ANDNE     *BLANK
058300021014     C                   SETON                                        32
058400021014     C                   MOVE      *ZERO         VABVLB
058500021015     C                   EVAL      vinmsg = %trimr(vinmsg)
058600021014     C                             + ' ' + 'VHBVLB'
058700021014     C                   ELSE
058800021014     C                   MOVEL     WI07          VABVLB
058900021014     C                   ENDIF
059000021014     C                   IF        WD07 = *blanks
059100021014     C                   EVAL      WD07 = *zeros
059200021014     C                   ENDIF
059300021014     C                   TESTN                   WD07                 30
059400021014     C  N30              TESTN                   WD07                   30
059500021014     C  N30              TESTN                   WD07                     30
059600021014     C                   MOVE      WD07          W001A             1
059700021014     C     *IN30         IFEQ      *OFF
059800021014     C     W001A         ORLT      '0'
059900021014     C     W001A         ANDNE     *BLANK
060000021014     C                   SETON                                        32
060100021014     C                   MOVE      *ZERO         VABVLB
060200021015     C                   EVAL      vinmsg = %trimr(vinmsg)
060300021014     C                             + ' ' + 'VHBVLB'
060400021014     C                   ELSE
060500021014     C                   MOVE      WD07          VABVLB
060600021014     C                   ENDIF
060700021014     C*
060800021014     C                   MOVE      *ZERO         VABQFT
060900021014     C                   MOVE      VHBQFT        WASC15
061000021014     C                   IF        WI15 = *blanks
061100021014     C                   EVAL      WI15 = *zeros
061200021014     C                   ENDIF
061300021014     C                   TESTN                   WI15                 30
061400021014     C  N30              TESTN                   WI15                   30
061500021014     C  N30              TESTN                   WI15                     30
061600021014     C                   MOVE      WI15          W001A             1
061700021014     C     *IN30         IFEQ      *OFF
061800021014     C     W001A         ORLT      '0'
061900021014     C     W001A         ANDNE     *BLANK
062000021014     C                   SETON                                        32
062100021014     C                   MOVE      *ZERO         VABQFT
062200021015     C                   EVAL      vinmsg = %trimr(vinmsg)
062300021014     C                             + ' ' + 'VHBQFT'
062400021014     C                   ELSE
062500021014     C                   MOVEL     WI15          VABQFT
062600021014     C                   ENDIF
062700021014     C                   IF        WD15 = *blanks
062800021014     C                   EVAL      WD15 = *zeros
062900021014     C                   ENDIF
063000021014     C                   TESTN                   WD15                 30
063100021014     C  N30              TESTN                   WD15                   30
063200021014     C  N30              TESTN                   WD15                     30
063300021014     C                   MOVE      WD15          W001A             1
063400021014     C     *IN30         IFEQ      *OFF
063500021014     C     W001A         ORLT      '0'
063600021014     C     W001A         ANDNE     *BLANK
063700021014     C                   SETON                                        32
063800021014     C                   MOVE      *ZERO         VABQFT
063900021015     C                   EVAL      vinmsg = %trimr(vinmsg)
064000021014     C                             + ' ' + 'VHBQFT'
064100021014     C                   ELSE
064200021014     C                   MOVE      WD15          VABQFT
064300021014     C                   ENDIF
064400021014     C*
064500021014     C                   MOVE      *ZERO         VABCAS
064600021014     C                   MOVE      VHBCAS        WASC15
064700021014     C                   IF        WI15 = *blanks
064800021014     C                   EVAL      WI15 = *zeros
064900021014     C                   ENDIF
065000021014     C                   TESTN                   WI15                 30
065100021014     C  N30              TESTN                   WI15                   30
065200021014     C  N30              TESTN                   WI15                     30
065300021014     C                   MOVE      WI15          W001A             1
065400021014     C     *IN30         IFEQ      *OFF
065500021014     C     W001A         ORLT      '0'
065600021014     C     W001A         ANDNE     *BLANK
065700021014     C                   SETON                                        32
065800021014     C                   MOVE      *ZERO         VABCAS
065900021015     C                   EVAL      vinmsg = %trimr(vinmsg)
066000021014     C                             + ' ' + 'VHBCAS'
066100021014     C                   ELSE
066200021014     C                   MOVEL     WI15          VABCAS
066300021014     C                   ENDIF
066400021014     C                   IF        WD15 = *blanks
066500021014     C                   EVAL      WD15 = *zeros
066600021014     C                   ENDIF
066700021014     C                   TESTN                   WD15                 30
066800021014     C  N30              TESTN                   WD15                   30
066900021014     C  N30              TESTN                   WD15                     30
067000021014     C                   MOVE      WD15          W001A             1
067100021014     C     *IN30         IFEQ      *OFF
067200021014     C     W001A         ORLT      '0'
067300021014     C     W001A         ANDNE     *BLANK
067400021014     C                   SETON                                        32
067500021014     C                   MOVE      *ZERO         VABCAS
067600021015     C                   EVAL      vinmsg = %trimr(vinmsg)
067700021014     C                             + ' ' + 'VHBCAS'
067800021014     C                   ELSE
067900021014     C                   MOVE      WD15          VABCAS
068000021014     C                   ENDIF
068100021014     C*
068200021014     C                   TESTN                   VHBRMN               30
068300021014     C  N30              TESTN                   VHBRMN                 30
068400021014     C  N30              TESTN                   VHBRMN                   30
068500021014     C                   MOVE      VHBRMN        W001A             1
068600021014     C     *IN30         IFEQ      *OFF
068700021014     C     W001A         ORLT      '0'
068800021014     C                   SETON                                        32
068900021014     C                   MOVE      *ZERO         VABRMN
069000021015     C                   EVAL      vinmsg = %trimr(vinmsg)
069100021014     C                             + ' ' + 'VHBRMN'
069200021014     C                   ELSE
069300021014     C                   MOVE      VHBRMN        VABRMN
069400021014     C                   ENDIF
069500021014     C*
069600021014     C                   TESTN                   VHBNCD               30
069700021014     C  N30              TESTN                   VHBNCD                 30
069800021014     C  N30              TESTN                   VHBNCD                   30
069900021014     C                   MOVE      VHBNCD        W001A             1
070000021014     C     *IN30         IFEQ      *OFF
070100021014     C     W001A         ORLT      '0'
070200021014     C     W001A         ANDNE     *BLANK
070300021014     C                   SETON                                        32
070400021014     C                   MOVE      *ZERO         VABNCD
070500021015     C                   EVAL      vinmsg = %trimr(vinmsg)
070600021014     C                             + ' ' + 'VHBNCD'
070700021014     C                   ELSE
070800021014     C                   MOVE      VHBNCD        VABNCD
070900021014     C                   ENDIF
071000021014     C*
071100021014     C                   TESTN                   VHBNCA               30
071200021014     C  N30              TESTN                   VHBNCA                 30
071300021014     C  N30              TESTN                   VHBNCA                   30
071400021014     C                   MOVE      VHBNCA        W001A             1
071500021014     C     *IN30         IFEQ      *OFF
071600021014     C     W001A         ORLT      '0'
071700021014     C     W001A         ANDNE     *BLANK
071800021014     C                   SETON                                        32
071900021014     C                   MOVE      *ZERO         VABNCA
072000021015     C                   EVAL      vinmsg = %trimr(vinmsg)
072100021014     C                             + ' ' + 'VHBNCA'
072200021014     C                   ELSE
072300021014     C                   MOVE      VHBNCA        VABNCA
072400021014     C                   ENDIF
072500021014     C*
072600021014     C                   TESTN                   VHBZNC               30
072700021014     C  N30              TESTN                   VHBZNC                 30
072800021014     C  N30              TESTN                   VHBZNC                   30
072900021014     C                   MOVE      VHBZNC        W001A             1
073000021014     C     *IN30         IFEQ      *OFF
073100021014     C     W001A         ORLT      '0'
073200021014     C     W001A         ANDNE     *BLANK
073300021014     C                   SETON                                        32
073400021014     C                   MOVE      *ZERO         VABZNC
073500021015     C                   EVAL      vinmsg = %trimr(vinmsg)
073600021014     C                             + ' ' + 'VHBZNC'
073700021014     C                   ELSE
073800021014     C                   MOVE      VHBZNC        VABZNC
073900021014     C                   ENDIF
074000021014     C*
074100021014     C                   TESTN                   VHBDCR               30
074200021014     C  N30              TESTN                   VHBDCR                 30
074300021014     C  N30              TESTN                   VHBDCR                   30
074400021014     C                   MOVE      VHBDCR        W001A             1
074500021014     C     *IN30         IFEQ      *OFF
074600021014     C     W001A         ORLT      '0'
074700021014     C     W001A         ANDNE     *BLANK
074800021014     C                   SETON                                        32
074900021014     C                   MOVE      *ZERO         VABDCR
075000021015     C                   EVAL      vinmsg = %trimr(vinmsg)
075100021014     C                             + ' ' + 'VHBDCR'
075200021014     C                   ELSE
075300021014     C                   MOVE      VHBDCR        VABDCR
075400021014     C                   ENDIF
075500021014     C*
075600021014     C                   TESTN                   VHBHCR               30
075700021014     C  N30              TESTN                   VHBHCR                 30
075800021014     C  N30              TESTN                   VHBHCR                   30
075900021014     C                   MOVE      VHBHCR        W001A             1
076000021014     C     *IN30         IFEQ      *OFF
076100021014     C     W001A         ORLT      '0'
076200021014     C     W001A         ANDNE     *BLANK
076300021014     C                   SETON                                        32
076400021014     C                   MOVE      *ZERO         VABHCR
076500021015     C                   EVAL      vinmsg = %trimr(vinmsg)
076600021014     C                             + ' ' + 'VHBHCR'
076700021014     C                   ELSE
076800021014     C                   MOVE      VHBHCR        VABHCR
076900021014     C                   ENDIF
077000021014     C*
077100021014     C                   MOVE      *ZERO         VABVMD
077200021014     C                   MOVE      VHBVMD        WASC15
077300021014     C                   IF        WI15 = *blanks
077400021014     C                   EVAL      WI15 = *zeros
077500021014     C                   ENDIF
077600021014     C                   TESTN                   WI15                 30
077700021014     C  N30              TESTN                   WI15                   30
077800021014     C  N30              TESTN                   WI15                     30
077900021014     C                   MOVE      WI15          W001A             1
078000021014     C     *IN30         IFEQ      *OFF
078100021014     C     W001A         ORLT      '0'
078200021014     C     W001A         ANDNE     *BLANK
078300021014     C                   SETON                                        32
078400021014     C                   MOVE      *ZERO         VABVMD
078500021015     C                   EVAL      vinmsg = %trimr(vinmsg)
078600021014     C                             + ' ' + 'VHBVMD'
078700021014     C                   ELSE
078800021014     C                   MOVEL     WI15          VABVMD
078900021014     C                   ENDIF
079000021014     C                   IF        WD15 = *blanks
079100021014     C                   EVAL      WD15 = *zeros
079200021014     C                   ENDIF
079300021014     C                   TESTN                   WD15                 30
079400021014     C  N30              TESTN                   WD15                   30
079500021014     C  N30              TESTN                   WD15                     30
079600021014     C                   MOVE      WD15          W001A             1
079700021014     C     *IN30         IFEQ      *OFF
079800021014     C     W001A         ORLT      '0'
079900021014     C     W001A         ANDNE     *BLANK
080000021014     C                   SETON                                        32
080100021014     C                   MOVE      *ZERO         VABVMD
080200021015     C                   EVAL      vinmsg = %trimr(vinmsg)
080300021014     C                             + ' ' + 'VHBVMD'
080400021014     C                   ELSE
080500021014     C                   MOVE      WD15          VABVMD
080600021014     C                   ENDIF
080700021014     C*
080800021014     C                   TESTN                   VHBANT               30
080900021014     C  N30              TESTN                   VHBANT                 30
081000021014     C  N30              TESTN                   VHBANT                   30
081100021014     C                   MOVE      VHBANT        W001A             1
081200021014     C     *IN30         IFEQ      *OFF
081300021014     C     W001A         ORLT      '0'
081400021014     C     W001A         ANDNE     *BLANK
081500021014     C                   SETON                                        32
081600021014     C                   MOVE      *ZERO         VABANT
081700021015     C                   EVAL      vinmsg = %trimr(vinmsg)
081800021014     C                             + ' ' + 'VHBANT'
081900021014     C                   ELSE
082000021014     C                   MOVE      VHBANT        VABANT
082100021014     C                   ENDIF
082200021014     C*
082300021014     C* CAMPI ALFABETICI:
082400021014     C                   MOVE      VHBATB        VABATB
082500021014     C                   MOVE      VHBRSD        VABRSD
082600021014     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
082700021014     C     '@':'A'       XLATE     VABRSD        VABRSD
082800021014     C* ==
082900021014     C                   MOVE      VHBRD2        VABRD2
083000021014     C                   MOVE      VHBIND        VABIND
083100021014     C                   MOVE      VHBCAD        VABCAD
083200021014     C                   MOVE      VHBLOD        VABLOD
083300021014     C                   MOVE      VHBPRD        VABPRD
083400021014     C                   MOVE      VHBNZD        VABNZD
083500021014     C                   MOVE      VHBGC1        VABGC1
083600021014     C                   MOVE      VHBGC2        VABGC2
083700021014     C                   MOVE      VHBTSP        VABTSP
083800021014     C                   MOVE      VHBVAS        VABVAS
083900021014     C                   MOVE      VHBNAS        VABNAS
084000021014     C                   MOVE      VHBTIC        VABTIC
084100021014     C                   MOVE      VHBVCA        VABVCA
084200021014     C                   MOVE      VHBGCA        VABGCA
084300021014     C                   MOVE      VHBRMA        VABRMA
084400021014     C                   MOVE      VHBXCO        VABXCO
084500021014     C                   MOVE      VHBNOT        VABNOT
084600021014     C                   MOVE      VHBNT2        VABNT2
084700021014     C                   MOVE      VHBFFD        VABFFD
084800021014     C                   MOVE      VHBTCR        VABTCR
084900021014     C                   MOVE      VHBCTS        VABCTS
085000021014     C                   MOVE      VHBFTM        VABFTM
085100021014     C                   MOVE      VHBVAD        VABVAD
085200021014     C                   MOVE      VHBGMA        VABGMA
085300021014     C                   MOVE      VHBGGA        VABGGA
085400021014     C                   MOVE      VHBGVA        VABGVA
085500021014     C                   MOVE      VHBTC1        VABTC1
085600021014     C                   MOVE      VHBTC2        VABTC2
085700021014     C                   MOVE      VHBSCL        VABSCL
085800021014     C                   MOVE      VHBRMO        VABRMO
085900021014     C                   MOVE      VHBCMO        VABCMO
086000021014     C                   MOVE      VHBNMO        VABNMO
086100021014     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
086200021014     C* NELLA NAZIONE ORIGINALE
086300021014     C                   MOVE      VHBNMO        WA1D              1
086400021014     C                   MOVEL     VHBNMO        WA1S              1
086500021014     C     WA1D          IFNE      *BLANK
086600021014     C     WA1S          ANDEQ     *BLANK
086700021014     C                   MOVE      *BLANK        VABNMO
086800021015     C                   ENDIF
086900020305     C*
087000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
087100011113     C                   EXSR      CHKIMPDIV
087200021016     C*
087300021016     C                   exsr      wrivat                                       => scrivo VAT
087400021016     C                   exsr      wrivab                                       => scrivo VAB
087500010202     C*
087600000801     C* Ebbene...
087700021015     C                   IF        *IN31 = *on  OR
087800021014     C                             *IN32 = *on
087900021014     C   31              ADD       1             §CTRNO
088000021014     C   32              ADD       1             §CTRMO
088100000801     C                   EVAL      recko = vindta
088200000801     C                   EXCEPT    rigadett
088300000801     C                   EVAL      vinflg = '2'
088400000801     C                   ELSE
088500010201     C                   ADD       1             §CTROKVB
088600000801     C                   ENDIF
088700021015     C                   ENDIF
088800021015     C                   ENDIF
088900000801     C*
089000000801     C                   ENDSR
089100010201     C*----------------------------------------------------*
089200021014     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
089300010201     C*----------------------------------------------------*
089400021014     C     WRIVAT        BEGSR
089500021016     C*
089600021014     C*
089700021014     C* Inizializzo i campi d wrk x il calcolo dei barcode
089800021016     C                   MOVEL     *blanks       wrkPrefix         2
089900021016     C                   Z-ADD     *zeros        wrkNOT_NumDA      8 0
090000021016     C                   Z-ADD     *zeros        wrkNOT_NumA       8 0
090100021016     C                   MOVEL     *blanks       wrkNOT_Alfa       8
090200021016     C                   Z-ADD     *zeros        wrkNOT_Num        8 0
090300021016     C                   Z-ADD     *zeros        numNOT            8 0
090400021016     C*
090500021016     C* Reperisco il frefisso alfa dei barcode
090600021016     C                   EVAL      wrkPrefix=%trim(%subst(vabNT2:1:2))
090700020801     C*
090800021014     C* Reperisco i barcode dal campo VABNT2: i primi 10 char indicano il barcode di partenza
090900021014     C* e i secondi 10 char indicano il barcode finale (della spedizione corrente)
091000021016     C                   EVAL      PiStr=%trim(%subst(vabNT2:3:8))
091100021014     C                   EXSR      CHKNUM
091200021014     C                   IF        PiInt=*on
091300021016     C                   Z-ADD     PiVal         wrkNOT_NumDA
091400021014     C                   ENDIF
091500021014     C*
091600021016     C                   EVAL      PiStr=%trim(%subst(vabNT2:13:8))
091700021014     C                   EXSR      CHKNUM
091800021014     C                   IF        PiInt=*on
091900021016     C                   Z-ADD     PiVal         wrkNOT_NumA
092000021014     C                   ENDIF
092100021014     C*
092200021016     C                   IF        wrkNOT_NumDA > *zeros AND
092300021016     C                             wrkNOT_NumA  > *zeros
092400021016     C                   DOW       (wrkNOT_NumA - wrkNOT_NumDA + 1) > numNOT
092500021016     C                   EVAL      vatFGS = vabFGS
092600021014     C                   EVAL      vatAAS = vabAAS
092700021014     C                   EVAL      vatCCM = vabCCM
092800021014     C                   EVAL      vatLNP = vabLNP
092900021014     C                   EVAL      vatNSP = vabNSP
093000021016     C                   EVAL      wrkNOT_Num = wrkNOT_NumDA + numNOT
093100021016     C                   MOVE(P)   wrkNOT_Num    wrkNOT_Alfa
093200021016     C                   EVAL      vatNOT = wrkPrefix + wrkNOT_Alfa
093300021014     C                   WRITE     FIVAT000
093400021015     C                   EVAL      numNOT = numNOT + 1
093500021014     C                   ENDDO
093600021014     C                   ENDIF
093700021016     C*
093800021016     C* Dopo aver costrito tutti i barcode abblenco il campo VABNT2
093900021016     C                   MOVEL     *blanks       vabNT2
094000010201     C*
094100010201     C                   ENDSR
094200010202     C*----------------------------------------------------*
094300021014     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
094400010202     C*----------------------------------------------------*
094500020305     C     PREVAT        BEGSR
094600010202     C*
094700021014     C* Compongo il nome del membro da dare al FIVATWWR
094800010202     C                   eval      parmbr = vlrhdl
094900010202     C                   movel     'M'           parmbr
095000021014     C                   eval      parccm = '00060948'
095100010202     C                   eval      paropz = '1'
095200010202     C* Effettuo la chiamata al CLLE preposto
095300050128     C                   call(e)   'TITVVTC'
095400010202     C                   parm                    parccm
095500010202     C                   parm                    parmbr
095600010202     C                   parm                    paropz
095700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
095800010202     C                   if        %error
095900010202     C                   movel     '1'           chkcall
096000010202     C                   else
096100010202     C                   movel     '0'           chkcall
096200010202     C                   endif
096300010202     C*
096400010202     C                   ENDSR
096500000801     C*----------------------------------------------------*
096600000801     C*  CONTROLLO NUMERICITA' CAMPI
096700000801     C*----------------------------------------------------*
096800000801     C     CHKNUM        BEGSR
096900000801     C*
097000000801     C                   call(e)   'ISNUMERIC'
097100000801     C                   PARM                    PiStr            30
097200020801     C                   PARM      ','           PiDecChr          1
097300000801     C                   PARM      *ZEROS        PiVal            30 9
097400000801     C                   PARM      '0'           PiInt             1
097500000801     C                   PARM      '0'           PiNum             1
097600000801     C                   IF        %error
097700000801     C                   EVAL      PiInt=*off
097800000801     C                   ENDIF
097900000801     C*
098000000801     C                   ENDSR
098100000801     C***
098200000801
098300011113
098400011113     C*----------------------------------------------------*
098500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
098600011113     C*----------------------------------------------------*
098700011113     C     CHKIMPDIV     BEGSR
098800011113     C*
098900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
099000011113     C                   Z-ADD     *zeros        wrkDec            9 9
099100011113     C*
099200011113     C* Come prima cosa effettuo considerazioni sulla divisa
099300011113     C                   IF        vabIAS > *zeros
099400011113     C                   IF        vabVAS <> 'EUR'
099500011113     C                   EVAL      vabVAS =  'ITL'
099600011113     C                   ENDIF
099700011113     C                   ENDIF
099800011113     C*
099900011113     C                   IF        vabCAS > *zeros
100000011113     C                   IF        vabVCA <> 'EUR'
100100011113     C                   EVAL      vabVCA =  'ITL'
100200011113     C                   ENDIF
100300011113     C                   ENDIF
100400011113     C*
100500011113     C                   IF        vabVMD > *zeros
100600020305     C                   IF        vabVAD <> 'EUR'
100700011113     C                   EVAL      vabVAD =  'ITL'
100800011113     C                   ENDIF
100900011113     C                   ENDIF
101000011113     C*
101100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
101200011113     C                   Z-ADD     vabIAS        wrkDec
101300011113     C                   IF        wrkDec > *zeros
101400011113     C                   IF        vabVAS = 'ITL'
101500011113     C                   EVAL      vabIAS = *zeros
101600011113     C                   ENDIF
101700011113     C                   ENDIF
101800011113     C*
101900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
102000011113     C                   Z-ADD     vabCAS        wrkDec
102100011113     C                   IF        wrkDec > *zeros
102200011113     C                   IF        vabVCA = 'ITL'
102300011113     C                   EVAL      vabCAS = *zeros
102400011113     C                   ENDIF
102500011113     C                   ENDIF
102600011113     C*
102700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
102800011113     C                   Z-ADD     vabVMD        wrkDec
102900011113     C                   IF        wrkDec > *zeros
103000011113     C                   IF        vabVAD = 'ITL'
103100011113     C                   EVAL      vabVMD = *zeros
103200011113     C                   ENDIF
103300011113     C                   ENDIF
103400011113     C*
103500011113     C                   ENDSR
103600011113     C***
103700021014
103800021014
103900021014
104000021014      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
104100021014     C     repfil        BEGSR
104200021014     C*
104300021014     C                   if        invfil = *zeros and
104400021014     C                             depfil > *zeros and
104500021014     C                             (vinflg = *blanks or
104600021014     C                              vinflg = *zeros)
104700021014     C
104800021014     C                   eval      invfil = depfil
104900021014     C                   endif
105000021014     C*
105100021014     C                   if        depfil <> invfil and
105200021014     C                             invfil > *zeros
105300021014     C                   eval      flgMulti = '1'
105400021014     C                   if        vinflg = *blanks
105500021014     C                   add       1             cntNonEl
105600021014     C                   endif
105700021014     C                   endif
105800021014     C*
105900021014     C                   if        vinflg = '2'
106000021014     C                   eval      flgStato = '2'
106100021014     C                   endif
106200021014     C*
106300021014     C                   ENDSR
106400021014     C***
106500011113
106600011113
106700000801
106800990920      /TITLE Invio dei dati al punto operativo.
106900010202     C     invio         BEGSR
107000990920     C*
107100021014     C* 1° invio FIVAT
107200010201     C                   reset                   dscmz
107300010201     C                   move      vlrpoi        cmzdst
107400021014     C                   eval      cmzfld = 'FIVATWWR'
107500010201     C                   eval      cmzmbd = vlrhdl
107600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
107700021009     C***                if        prmfir = *blanks
107800021014     C                   eval      cmzfla = 'FIVAT00F'
107900021014     C                   eval      cmzmba = 'FIVAT00F'
108000021009     C***                else
108100021009     C***                eval      cmzfla = prmfir
108200021009     C***                eval      cmzmba = prmfir
108300021009     C***                endif
108400010201     C                   eval      cmznrr = *zeros
108500020305     C                   move      §ctrokvt      cmznrr
108600021018     C                   eval      cmzlba = vlrfl1
108700010201     C                   call(e)   'TIS711C'
108800010201     C                   parm                    dscmz
108900010201     C                   parm      *blanks       esito
109000010205     C                   if        %error
109100010205     C                             or cmzerr = '1'
109200010205     C                             or esito  = '1'
109300010205     C                   eval      wrkesito = '3'
109400010205     C                   else
109500010201     C*
109600021014     C* 2° invio FIVAB
109700010201     C                   reset                   dscmz
109800010201     C                   move      vlrpoi        cmzdst
109900010201     C                   eval      cmzfld = vlrfou
110000010201     C                   eval      cmzmbd = vlrhdl
110100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
110200021009     C***                if        prmfir = *blanks
110300021014     C                   eval      cmzfla = 'FIVAB00F'
110400021014     C                   eval      cmzmba = 'FIVAB00F'
110500021009     C***                else
110600021009     C***                eval      cmzfla = prmfir
110700021009     C***                eval      cmzmba = prmfir
110800021009     C***                endif
110900010201     C                   eval      cmznrr = *zeros
111000010201     C                   move      §ctrokvb      cmznrr
111100021018     C                   eval      cmzlba = vlrfl1
111200010201     C                   call(e)   'TIS711C'
111300010201     C                   parm                    dscmz
111400010201     C                   parm      *blanks       esito
111500010201     C                   if        %error
111600010201     C                             or cmzerr = '1'
111700010201     C                             or esito  = '1'
111800010201     C                   eval      wrkesito = '3'
111900010201     C                   endif
112000010205     C                   endif
112100990920     C*
112200000613     C                   ENDSR
112300000613     C***
112400021014
112500021014
112600021014
112700021014      /TITLE Invio dei dati al punto operativo.
112800021014     C     opeini        BEGSR
112900021014     C*
113000021014     C* Inizializzo flag e contatori operativi
113100021014     C                   movel     '0'           flgGiro           1
113200021014     C                   movel     '0'           flgMulti          1
113300021014     C                   movel     '1'           flgStato          1
113400021014     C                   movel     '0'           flgOk             1
113500021014     C                   z-add     *zeros        cntNonEl         10 0
113600021014     C                   z-add     *zeros        depfil            3 0
113700021014     C                   z-add     *zeros        invfil            3 0
113800021014     C*
113900021014     C                   ENDSR
114000021014     C***
114100070411
114200070411     C     *pssr         BEGSR
114300070411     C*
114400070411     C                   if        %open(tivin00r)
114500070411     C                   close     tivin00r
114600070411     C                   endif
114700070411     C                   if        %open(fivabwwr)
114800070411     C                   close     fivabwwr
114900070411     C                   endif
115000070411     C                   if        %open(fivatwwr)
115100070411     C                   close     fivatwwr
115200070411     C                   endif
115300070411     C*
115400070411     C* Effettuo la chiamata al CLLE preposto
115500070411     C                   call(e)   'TITVVTC'
115600070411     C                   parm                    parccm
115700070411     C                   parm                    parmbr
115800070411     C                   parm      '2'           paropz
115900070411     C*
116000070411     C                   eval      wrkesito = '2'
116100070411     C*
116200070411     C                   seton                                        LR
116300070411     C*
116400070411     C                   ENDSR     '*CANCL'
116500070411     C***
116600070411
116700021014
116800990910
116900000613     C     *inzsr        BEGSR
117000990910     C*
117100990910     C     *entry        plist
117200990920     C                   parm                    tivlrds
117300990921     C                   parm      wrkesito      esito
117400000724     C                   parm                    prmlit
117500000710     C                   parm                    prmfir
117600000613     C*
117700000830     C* CALCOLA LA DATA CORRENTE
117800000830     C                   time                    wn14             14 0
117900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
118000000830     C                   z-add     wn8           g08dat
118100000830     C                   z-add     *zeros        g08inv
118200000830     C                   movel     '0'           g08err
118300000830     C                   call      'XSRDA8'
118400000830     C                   parm                    wlbda8
118500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
118600000830     C*
118700000613     C                   ENDSR
118800000613     C***
118900990908
119000021015     Otitv25p   E            riepilogo         2
119100990915     O                                              'Upload via Internet'
119200990915     O                                           +1 'Traduzione TIVIN00R -'
119300021014     O                                           +1 'FIVABWWR/FIVATWWR'
119400010201     O                                           +1 'Report testate bolle'
119500990915     O          E            riepilogo   2
119600990915     O                       wrkdata
119700990915     O                       wrkora              +1
119800990915     O                       procname            +1
119900990915     O          E            riepilogo   2
120000990915     O                                              'Cliente..................:'
120100990915     O                       VABCCM        z     +1
120200990915     O          E            riepilogo   2
120300990920     O                                              'Riferimento Strategi.....:'
120400990920     O                       vlrhdl              +1
120500990915     O          E            riepilogo   2
120600990915     O                                              'Giusti...................:'
120700010201     O                       §CTROKVB      2   +  1
120800990915     O          E            riepilogo   2
120900990915     O                                              'Sbagliati e corretti.....:'
121000971022     O                       §CTRMO        2   +  1
121100990915     O          E            riepilogo   2
121200990915     O                                              'Sbagliati e scartati.....:'
121300971022     O                       §CTRNO        2   +  1
121400000613
121500021015     Otitv25ps  E            testdett          2
121600000613     O                                              'Upload via Internet'
121700000613     O                                           +1 'Traduzione TIVIN00R -'
121800021014     O                                           +1 'FIVABWWR/FIVATWWR'
121900010201     O                                           +1 'Report testate bolle'
122000000616     O          E            testdett    3
122100000613     O                                           +2 'N° rec'
122200000613     O                                           +3 'Anteprima contenuto'
122300000616     O          E            rigadett    2
122400000613     O                       rrnum               +2
122500000621     O                       recko               +3
122600000616     O          E            findett     2
122700000613     O                       wrkdata
122800000613     O                       wrkora              +1
122900000613     O                       procname            +1
123000000616     O          E            findett     2
123100000613     O                                              'Cliente..................:'
123200000613     O                       VABCCM        z     +1
123300000616     O          E            findett     2
123400000613     O                                              'Riferimento Strategi.....:'
123500000613     O                       vlrhdl              +1
123600000616     O          E            findett     2
123700000613     O                                              'Giusti...................:'
123800010201     O                       §CTROKVB      2   +  1
123900000616     O          E            findett     2
124000000613     O                                              'Sbagliati e corretti.....:'
124100000613     O                       §CTRMO        2   +  1
124200000616     O          E            findett     2
124300000613     O                                              'Sbagliati e scartati.....:'
124400000613     O                       §CTRNO        2   +  1
124500000512** CMD - COMANDI CL
124600021015OVRPRTF FILE(TITV25P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
124700021015OVRPRTF FILE(TITV25PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
124800021015DLTOVR FILE(TITV25P TITV25PS) LVL(*)
124900000512
125000000512
