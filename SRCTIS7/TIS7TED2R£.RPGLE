000100050613      /TITLE Upload via Internet: traduzione in EDIVADWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050613     FEDIVADwr  O    E             DISK    usropn
000700050613     Ftis7tedp  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900050613     Ftis7tedps O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700010330     D wrkdata         s               d
002800010330     D wrkora          s               t
002900010330     D rrnum           s              6  0 INZ(*zeros)
003000010330     D recko           s            150    INZ(*blanks)
003100011123     D depcmd          s            150    INZ(*blanks)
003200050613     D wNomeFile       s             30    INZ(*blanks)
003300050613     D wCMR            s             35    INZ(*blanks)
003400050613     D wPosDaA         s              2    INZ(*blanks)
003500050613     D wPosDa          s              2  0 INZ(*zeros)
003600050613     D wLungA          s              2    INZ(*blanks)
003700050613     D wLung           s              2  0 INZ(*zeros)
003800010330     D*------------------
003900010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004000010330     D*------------------
004100010330     D WLBDA8          DS                  INZ
004200010330     D  G08DAT                 1      8  0
004300010330     D  G08INV                 9     16  0
004400010330     D  G08ERR                17     17
004500010330     D  G08TGI                18     22  0
004600000313     D*
004700051212
004800051212     D TxtInOut        S           2048
004900051212     D ElencoChar      S            256
005000051212     D TipoElenco      S              1
005100051212     D CharSost        S              1
005200051212     D UpperCase       S              1
005300051212     D ChkNull         S              1
005400051212     D CharNull        S              1
005500051212     D EsitoBon        S              1
005600010330
005700010330
005800010330
005900990915     C                   time                    wrkdata
006000990915     C                   time                    wrkora
006100000913     C                   reset                   rrnum
006200990921     C                   reset                   esito
006300990921     C                   reset                   wrkesito
006400010601     C*
006500010601     C                   exsr      opeini
006600010601     C                   exsr      rwvad
006700010601     C*
006800010601     C                   seton                                        lr
006900010601
007000010601
007100010601
007200010601
007300010601     C*--------------------------------------------------------
007400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007500010601     C*--------------------------------------------------------
007600010601     C     PREELA        BEGSR
007700010601     C*
007800010601     C* SE OCCORRE SPEDIRE IN FILIALE
007900010601     C                   if        invfil <> *zeros and
008000010601     C                             flgGiro = '0'
008100010601     C*
008200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008300010601     C                   eval      flgGiro = '1'
008400000724     C*
008500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008700010601     C     invfil        chain     azorg01l
008800000724     C                   if        %found
008900000724     C                   movel(p)  CMD(1)        depcmd
009000020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009100000724     C*
009200000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009300011123     C                   Z-ADD     150           LENGH            15 5
009400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009500000724     C                   PARM                    depcmd
009600000724     C                   PARM                    LENGH
009700000724     C*
009800000724     C                   endif
009900000616     C*
010000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010100000616     C                   movel(p)  CMD(2)        depcmd
010200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010300000616     C*
010400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010500011123     C                   Z-ADD     150           LENGH            15 5
010600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010700000616     C                   PARM                    depcmd
010800000616     C                   PARM                    LENGH
010900000613     C*
011000050613     C                   if        not %open(tis7tedps)
011100050613     C                   open      tis7tedps
011200030407     C   50              except    testdett
011300000613     C                   endif
011400010601     C*
011500010601     C                   endif
011600010601     C*
011700010601     C                   ENDSR
011800010601     C***
011900010601
012000010601
012100010601
012200010601
012300010601
012400010601
012500010601
012600010601     C*--------------------------------------------------------
012700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012800010601     C*--------------------------------------------------------
012900010601     C     ENDELA        BEGSR
013000990915     C*
013100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013200000613     C*
013300050613     C                   if        %open(tis7tedps)
013400030407     C   50              except    findett
013500050613     C                   close     tis7tedps
013600000613     C                   endif
013700000616     C*
013800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013900011123     C                   Z-ADD     150           LENGH            15 5
014000010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
014100000616     C                   PARM                    CMD(3)
014200000616     C                   PARM                    LENGH
014300000616     C*
014400010601     C                   ENDSR
014500010601     C***
014600010601
014700010601
014800010601
014900010601
015000000613
015100000613     C*--------------------------------------------------------
015200000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015300000613     C*--------------------------------------------------------
015400000621     C     STPR          BEGSR
015500000613     C*
015600050613     C                   if        not %open(tis7tedp)
015700050613     C                   open      tis7tedp
015800990915     C                   endif
015900990915     C*
016000030407     C   50              except    riepilogo
016100990915     C*
016200050613     C                   if        %open(tis7tedp)
016300050613     C                   close     tis7tedp
016400990914     C                   endif
016500000613     C*
016600000613     C                   ENDSR
016700000613     C***
016800000613
016900010330
017000010601
017100010601
017200010601
017300010330     C*--------------------------------------------------------
017400050613     C* RWVAD   LEGGE tivin00r E SCRIVE EDIVADWF              *
017500010330     C*--------------------------------------------------------
017600010330     C     RWVAD         BEGSR
017700010330     C*
017800050613     C*
017900050613     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
018000050613     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
018100050613     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
018200050613     C                   if        *in60 = *on
018300010330     C                   if        not %open(tivin00r)
018400010330     C                   open      tivin00r
018500010330     C                   endif
018600050613     C                   if        not %open(edivadwr)
018700050613     C                   open      edivadwr
018800010330     C                   endif
018900010330     C*
019000010604     C                   clear                   §CTROK
019100010604     C                   clear                   §CTRMO
019200010604     C                   clear                   §CTRNO
019300010330     C*
019400010330     C                   DO        *HIVAL
019500010330     C*
019600010330     C                   READ      tivin00r                               70
019700010618     C*
019800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019900010618     C                   if        vinflg = '1'
020000010618     C                   eval      flgOk = '1'
020100010618     C                   endif
020200010618     C*
020300010330     C                   if        vindta > *blanks
020400010330     C                   add       1             rrnum
020500010330     C*
020600010601     C                   if        *in70 = *off and
020700010330     C                             (vinflg = *blanks
020800010330     C                              or vinflg = '0'
020900010330     C                              or vinflg = '2')
021000010330     C*
021100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021200010711     C                   if        vinflg = *blanks or vinflg = '0'
021300010711     C                   clear                   vinmsg
021400010711     C                   endif
021500010601     C*
021600010330     C                   exsr      inzvar
021700010330     C                   exsr      defcam
021800010330     C                   exsr      impvad
021900050613     C*
022000050613     C* VALORIZZO CAMPI RELATIVI AL "CMR"
022100050613     C                   EVAL      VADCMR = wCMR
022200050613     C                   EVAL      VADCNT = 1
022300010601     C*
022400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022500101209     C                   eval      depfil = VADFGS
022600010601     C                   exsr      repfil
022700010601     C                   if        depfil = invfil
022800021014     C                   if        vlrpoi = 999
022900101209     C                   MOVE(P)   invfil        VADFGS
023000021014     C                   else
023100021014     C                   MOVE(P)   vlrpoi        VADFGS
023200021014     C                   endif
023300010601     C*
023400010601     C                   exsr      PREELA
023500051212
023600051212      * "bonifico" i dati di input da eventuali caratteri indesiderati
023700051212     c                   exsr      exeBon
023800051212
023900010601     C*
024000010604     C* Ebbene...
024100010604     C*
024200010604     C  N31              ADD       1             §CTROK            7 0
024300010604     C   32              ADD       1             §CTRMO            7 0
024400010604     C   31              ADD       1             §CTRNO            7 0
024500050613     C  N31              WRITE     EDIVAD00
024600010604     C*
024700010604     C                   if        *in31 = *off and
024800010604     C                             *in32 = *off
024900010604     C                   eval      vinflg = '1'
025000010604     C                   else
025100010604     C                   eval      recko = vindta
025200050613     C                   if        %open(tis7tedps)
025300030407     C   50              except    rigadett
025400010612     C                   endif
025500010604     C                   eval      vinflg = '2'
025600010604     C                   endif
025700010604     C                   endif
025800010601     C*
025900010604     C                   endif
026000010604     C*
026100010330     C                   else
026200010330     C                   eval      vinflg = '1'
026300010330     C                   endif
026400010601     C*
026500010601     C  N70              update    tivin000
026600010330     C*
026700010330     C  N70              ENDdo
026800010601     C
026900010601     C*
027000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027100010601     C                   if        cntNonEl = *zeros or
027200010601     C                             flgMulti = '0'
027300010330     C* Se non ci sono record con errori ...
027400010601     C                   if        §ctrno = 0 and
027500010604     C                             §ctrmo = 0 and
027600010601     C                             flgStato <> '2'
027700010330     C* ... restituisco esito OK.
027800010330     C                   eval      wrkesito = '0'
027900010330     C                   else
028000010330     C                   if        §ctrok > 0
028100010330     C                   eval      wrkesito = '1'
028200010330     C                   else
028300010615     C                   if        flgOk = '0'
028400010615     C                   eval      wrkesito = '2'
028500010615     C                   else
028600010615     C                   eval      wrkesito = '6'
028700010615     C                   endif
028800010330     C                   endif
028900010330     C                   endif
029000010601     C                   else
029100010601     C                   eval      wrkesito = '9'
029200010601     C                   endif
029300050613     C*
029400050613     C                   else
029500050613     C                   eval      wrkesito = '8'
029600050613     C                   eval      flgGiro = '1'
029700050613     C                   endif
029800010330     C*
029900010330     C                   if        %open(tivin00r)
030000010330     C                   close     tivin00r
030100010330     C                   endif
030200050613     C                   if        %open(edivadwr)
030300050613     C                   close     edivadwr
030400010330     C                   endif
030500010601     C*
030600010601     C                   if        vlrpoi <> 999
030700010601     C                   eval      invfil = vlrpoi
030800010601     C                   endif
030900010330     C*
031000010330     C                   if        §ctrok > 0
031100010601     C                             and invfil > *zeros
031200010330     C                   exsr      invio
031300010330     C                   endif
031400010612     C*
031500010612     C                   if        flgGiro = '1'
031600010612     C                   exsr      endela
031700010612     C                   endif
031800010330     C*
031900010330     C                   ENDSR
032000010330     C***
032100010330
032200010601
032300010601
032400010601
032500010601
032600010330     C*----------------------------------------------------*
032700010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032800010330     C*----------------------------------------------------*
032900010330     C     INZVAR        BEGSR
033000010330     C*
033100010330     C*
033200010330     C                   ENDSR
033300010330     C*----------------------------------------------------*
033400010330     C*  IMPOSTAZIONE CAMPI COSTANTI
033500010330     C*----------------------------------------------------*
033600010330     C     DEFCAM        BEGSR
033700010330     C*
033800050613     C                   CLEAR                   EDIVAD00
033900010330     C*
034000010330     C                   ENDSR
034100010330     C*----------------------------------------------------*
034200050613     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAD)
034300010330     C*----------------------------------------------------*
034400010330     C     IMPVAD        BEGSR
034500010330     C*
034600010604     C                   SETOFF                                       3132
034700010330     C*
034800010330     C* Reperimento campi ALFA
034900010330     C                   EVAL      VADATB=%trim(%subst(vindta:1:1))
035000101209     C                   EVAL      VADCDP=%trim(%subst(vindta:28:15))
035100101209     C*
035200101209     C                   EVAL      PiStr=%trim(%subst(vindta:2:3))
035300101209     C                   EXSR      CHKNUM
035400101209     C                   IF        PiInt=*on
035500101209     C                   Z-ADD     PiVal         VADFGS
035600101209     C                   ELSE
035700101209     C                   SETON                                        31
035800101209     C                   EVAL      vinmsg = %trimr(vinmsg)
035900101209     C                             + ' ' + 'VADFGS'
036000101209     C                   ENDIF
036100010330     C*
036200101209     C                   EVAL      PiStr=%trim(%subst(vindta:5:7))
036300010330     C                   EXSR      CHKNUM
036400010330     C                   IF        PiInt=*on
036500010330     C                   Z-ADD     PiVal         VADCCM
036600010330     C                   ELSE
036700010604     C                   SETON                                        31
036800010330     C                   EVAL      vinmsg = %trimr(vinmsg)
036900010330     C                             + ' ' + 'VADCCM'
037000010330     C                   ENDIF
037100010330     C*
037200101209     C                   EVAL      PiStr=%trim(%subst(vindta:12:3))
037300010330     C                   EXSR      CHKNUM
037400010330     C                   IF        PiInt=*on
037500010330     C                   Z-ADD     PiVal         VADLNP
037600010330     C                   ELSE
037700010604     C                   SETON                                        31
037800010330     C                   EVAL      vinmsg = %trimr(vinmsg)
037900021010     C                             + ' ' + 'VADLNP'
038000010330     C                   ENDIF
038100050104     C*
038200101209     C                   IF        %subst(vindta:15:4) <> *blanks
038300101209     C                   EVAL      PiStr=%trim(%subst(vindta:15:4))
038400050104     C                   EXSR      CHKNUM
038500050104     C                   IF        PiInt=*on
038600050104     C                   Z-ADD     PiVal         VADAAS
038700050104     C                   ELSE
038800050104     C                   SETON                                        31
038900050104     C                   EVAL      vinmsg = %trimr(vinmsg)
039000050104     C                             + ' ' + 'VADAAS'
039100050104     C                   ENDIF
039200050104     C                   ELSE
039300050104     C                   MOVEL     datcor        VADAAS
039400050104     C                   ENDIF
039500010330     C*
039600101209     C                   EVAL      PiStr=%trim(%subst(vindta:19:2))
039700010330     C                   EXSR      CHKNUM
039800010330     C                   IF        PiInt=*on
039900010330     C                   Z-ADD     PiVal         VADNRS
040000010330     C                   ELSE
040100010604     C                   SETON                                        32
040200010604     C                   Z-ADD     *zeros        VADNRS
040300010330     C                   EVAL      vinmsg = %trimr(vinmsg)
040400010330     C                             + ' ' + 'VADNRS'
040500010330     C                   ENDIF
040600010330     C*
040700101209     C                   EVAL      PiStr=%trim(%subst(vindta:21:7))
040800010330     C                   EXSR      CHKNUM
040900010330     C                   IF        PiInt=*on
041000010330     C                   Z-ADD     PiVal         VADNSP
041100010330     C                   ELSE
041200010604     C                   SETON                                        32
041300010604     C                   Z-ADD     *zeros        VADNSP
041400010330     C                   EVAL      vinmsg = %trimr(vinmsg)
041500010330     C                             + ' ' + 'VADNSP'
041600010330     C                   ENDIF
041700010330     C*
041800101209     C                   EVAL      PiStr=%trim(%subst(vindta:43:5))
041900010330     C                   EXSR      CHKNUM
042000010330     C                   IF        PiInt=*on
042100010330     C                   Z-ADD     PiVal         VADNCL
042200010330     C                   ELSE
042300010604     C                   SETON                                        32
042400010604     C                   Z-ADD     *zeros        VADNCL
042500010330     C                   EVAL      vinmsg = %trimr(vinmsg)
042600010330     C                             + ' ' + 'VADNCL'
042700010330     C                   ENDIF
042800010330     C*
042900101209     C                   EVAL      PiStr=%trim(%subst(vindta:48:7))
043000010330     C                   EXSR      CHKNUM
043100010330     C                   IF        PiInt=*on
043200010330     C                   Z-ADD     PiVal         VADNCD
043300010330     C                   ELSE
043400010604     C                   SETON                                        32
043500010604     C                   Z-ADD     *zeros        VADNCD
043600010330     C                   EVAL      vinmsg = %trimr(vinmsg)
043700010330     C                             + ' ' + 'VADNCD'
043800010330     C                   ENDIF
043900010330     C*
044000101209     C                   EVAL      PiStr=%trim(%subst(vindta:55:7))
044100010330     C                   EXSR      CHKNUM
044200010330     C                   IF        PiInt=*on
044300010330     C                   Z-ADD     PiVal         VADNCA
044400010330     C                   ELSE
044500010604     C                   SETON                                        32
044600010604     C                   Z-ADD     *zeros        VADNCA
044700010330     C                   EVAL      vinmsg = %trimr(vinmsg)
044800010330     C                             + ' ' + 'VADNCA'
044900010330     C                   ENDIF
045000010330     C*
045100010330     C                   ENDSR
045200990920
045300010601
045400010601
045500010330
045600010330
045700010330     C*----------------------------------------------------*
045800010330     C*  CONTROLLO NUMERICITA' CAMPI
045900010330     C*----------------------------------------------------*
046000010330     C     CHKNUM        BEGSR
046100010330     C*
046200010330     C                   call(e)   'ISNUMERIC'
046300010330     C                   PARM                    PiStr            30
046400010330     C                   PARM      '.'           PiDecChr          1
046500010330     C                   PARM      *ZEROS        PiVal            30 9
046600010330     C                   PARM      '0'           PiInt             1
046700010330     C                   PARM      '0'           PiNum             1
046800010330     C                   IF        %error
046900010330     C                   EVAL      PiInt=*off
047000010330     C                   ENDIF
047100010330     C*
047200010330     C                   ENDSR
047300010330     C***
047400010330
047500010601
047600010601
047700010601
047800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
047900010601     C     repfil        BEGSR
048000010601     C*
048100010601     C                   if        invfil = *zeros and
048200010601     C                             depfil > *zeros and
048300010613     C                             (vinflg = *blanks or
048400010613     C                              vinflg = *zeros)
048500010601     C
048600010601     C                   eval      invfil = depfil
048700010601     C                   endif
048800010601     C*
048900010601     C                   if        depfil <> invfil and
049000010601     C                             invfil > *zeros
049100010601     C                   eval      flgMulti = '1'
049200010601     C                   if        vinflg = *blanks
049300010601     C                   add       1             cntNonEl
049400010601     C                   endif
049500010601     C                   endif
049600010601     C*
049700010601     C                   if        vinflg = '2'
049800010601     C                   eval      flgStato = '2'
049900010601     C                   endif
050000010601     C*
050100010601     C                   ENDSR
050200010601     C***
050300010601
050400010601
050500010601
050600010330
050700010330
050800010330
050900990920      /TITLE Invio dei dati al punto operativo.
051000000613     C     invio         BEGSR
051100990920     C*
051200990920     C                   reset                   dscmz
051300010601     C                   move      invfil        cmzdst
051400990920     C                   eval      cmzfld = vlrfou
051500990920     C                   eval      cmzmbd = vlrhdl
051600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
051700000710     C                   if        prmfir = *blanks
051800050613     C                   eval      cmzfla = 'EDIVAD0F'
051900050613     C                   eval      cmzmba = 'EDIVAD0F'
052000000710     C                   else
052100000710     C                   eval      cmzfla = prmfir
052200000710     C                   eval      cmzmba = prmfir
052300000710     C                   endif
052400990920     C                   eval      cmznrr = *zeros
052500990920     C                   move      §ctrok        cmznrr
052600021003     C                   eval      cmzlba = vlrfl1
052700990920     C                   call(e)   'TIS711C'
052800990920     C                   parm                    dscmz
052900990921     C                   parm      *blanks       esito
053000990920     C                   if        %error
053100990920     C                             or cmzerr = '1'
053200990921     C                             or esito  = '1'
053300000710     C                   eval      wrkesito = '3'
053400990920     C                   endif
053500990920     C*
053600000613     C                   ENDSR
053700990910
053800010601
053900010601
054000010601
054100010601
054200010601      /TITLE Invio dei dati al punto operativo.
054300010601     C     opeini        BEGSR
054400010601     C*
054500010601     C* Inizializzo flag e contatori operativi
054600010601     C                   movel     '0'           flgGiro           1
054700010601     C                   movel     '0'           flgMulti          1
054800010601     C                   movel     '1'           flgStato          1
054900010615     C                   movel     '0'           flgOk             1
055000010601     C                   z-add     *zeros        cntNonEl         10 0
055100010601     C                   z-add     *zeros        depfil            3 0
055200010601     C                   z-add     *zeros        invfil            3 0
055300030407     C*
055400030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
055500030407     C                   if        %subst(vlrppt:1:1) = 'S'
055600030407     C                   seton                                        50
055700030407     C                   else
055800030407     C                   setoff                                       50
055900030407     C                   endif
056000010601     C*
056100010601     C                   ENDSR
056200010601     C***
056300010601
056400051212
056500051212
056600051212      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
056700051212     C     exeBon        BEGSR
056800051212     C*
056900051212     C* Imposto i parametri x il *pgm d "bonifica"
057000051212     C                   EVAL      ElencoChar = *blanks
057100051212     C                   EVAL      TipoElenco = *blanks
057200051212     C                   EVAL      CharSost   = *blanks
057300051212     C                   EVAL      UpperCase  = '1'
057400051212     C                   EVAL      ChkNull    = '1'
057500051212     C                   EVAL      CharNull   = *blanks
057600051212     C*
057700051212     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VADCDP
057800051212     C                   EVAL      TxtInOut   = VADCDP
057900051212     C                   EVAL      EsitoBon   = *blanks
058000051212     C*
058100051212     C                   CALL      'XCHKCHAR'
058200051212     C                   PARM                    TxtInOut
058300051212     C                   PARM                    ElencoChar
058400051212     C                   PARM                    TipoElenco
058500051212     C                   PARM                    CharSost
058600051212     C                   PARM                    UpperCase
058700051212     C                   PARM                    ChkNull
058800051212     C                   PARM                    CharNull
058900051212     C                   PARM                    EsitoBon
059000051212     C*
059100051212     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
059200051212     C                   IF        EsitoBon = '1'
059300051212     C                   EVAL      VADCDP = TxtInOut
059400051212     C                   ENDIF
059500051212     C*
059600051212     C                   ENDSR
059700010601
059800010601
059900010330
060000010330
060100000613     C     *inzsr        BEGSR
060200990910     C*
060300990910     C     *entry        plist
060400990920     C                   parm                    tivlrds
060500990921     C                   parm      wrkesito      esito
060600000724     C                   parm                    prmlit
060700000710     C                   parm                    prmfir
060800050613     C*
060900050613     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
061000050613     C                   move(p)   vlrMSG        wNomeFile
061100050613     C*
061200050613     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
061300050613     C* del codice CMR dal nome del file importato.
061400050613     C                   if        wNomeFile <> *blanks AND
061500050613     C                             vlrppt<>*blanks
061600050613     C                   if        %subst(vlrppt:1:1) = 'S'
061700050613     C                   seton                                        50
061800050613     C                   else
061900050613     C                   setoff                                       50
062000050613     C                   endif
062100050613     C                   eval      wPosDaA = %subst(vlrppt:2:2)
062200050613     C                   move(p)   wPosDaA       wPosDa
062300050613     C                   eval      wLungA  = %subst(vlrppt:4:2)
062400050613     C                   move(p)   wLungA        wLung
062500050613     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
062600050613     C                   seton                                        60
062700050613     C                   else
062800050613     C                   setoff                                       60
062900050613     C                   endif
063000010330     C*
063100010330     C* CALCOLA LA DATA CORRENTE
063200010330     C                   time                    wn14             14 0
063300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063400010330     C                   z-add     wn8           g08dat
063500010330     C                   z-add     *zeros        g08inv
063600010330     C                   movel     '0'           g08err
063700010330     C                   call      'XSRDA8'
063800010330     C                   parm                    wlbda8
063900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
064000000613     C*
064100000613     C                   ENDSR
064200000613     C***
064300990908
064400050613     Otis7tedp  E            riepilogo         2
064500990915     O                                              'Upload via Internet'
064600990915     O                                           +1 'Traduzione TIVIN00R -'
064700050613     O                                           +1 'EDIVADWR'
064800990915     O                                           +1 'Testate bolle'
064900990915     O          E            riepilogo   2
065000990915     O                       wrkdata
065100990915     O                       wrkora              +1
065200990915     O                       procname            +1
065300990915     O          E            riepilogo   2
065400990915     O                                              'Cliente..................:'
065500010330     O                       VADCCM        z     +1
065600990915     O          E            riepilogo   2
065700990920     O                                              'Riferimento Strategi.....:'
065800990920     O                       vlrhdl              +1
065900990915     O          E            riepilogo   2
066000990915     O                                              'Giusti...................:'
066100971022     O                       §CTROK        2   +  1
066200990915     O          E            riepilogo   2
066300990915     O                                              'Sbagliati e corretti.....:'
066400971022     O                       §CTRMO        2   +  1
066500990915     O          E            riepilogo   2
066600990915     O                                              'Sbagliati e scartati.....:'
066700971022     O                       §CTRNO        2   +  1
066800000613
066900050613     Otis7tedps E            testdett          2
067000000613     O                                              'Upload via Internet'
067100000613     O                                           +1 'Traduzione TIVIN00R -'
067200050613     O                                           +1 'EDIVADWR'
067300000613     O                                           +1 'Testate bolle'
067400000616     O          E            testdett    3
067500000613     O                                           +2 'N° rec'
067600000613     O                                           +3 'Anteprima contenuto'
067700000616     O          E            rigadett    2
067800000613     O                       rrnum               +2
067900000621     O                       recko               +3
068000000616     O          E            findett     2
068100000613     O                       wrkdata
068200000613     O                       wrkora              +1
068300000613     O                       procname            +1
068400000616     O          E            findett     2
068500000613     O                                              'Cliente..................:'
068600010330     O                       VADCCM        z     +1
068700000616     O          E            findett     2
068800000613     O                                              'Riferimento Strategi.....:'
068900000613     O                       vlrhdl              +1
069000000616     O          E            findett     2
069100000613     O                                              'Giusti...................:'
069200000613     O                       §CTROK        2   +  1
069300000616     O          E            findett     2
069400000613     O                                              'Sbagliati e corretti.....:'
069500000613     O                       §CTRMO        2   +  1
069600000616     O          E            findett     2
069700000613     O                                              'Sbagliati e scartati.....:'
069800000613     O                       §CTRNO        2   +  1
069900000613** CMD - COMANDI CL
070000060403OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7TEDP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
070100060403OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7TEDPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
070200050613DLTOVR FILE(TIS7TEDP TIS7TEDPS) LVL(*)
070300000613
070400000613
