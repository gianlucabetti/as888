000100050613      /TITLE Upload via Internet: traduzione in EDIVADWR.
000200081202     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
005700081202
005800081202     D*------------------
005900081202     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
006000081202     D*------------------
006100081202     D PiStr           S            100A   INZ
006200081202     D PiDecChr        S              1A   INZ
006300081202     D PiVal           S             63S30 INZ
006400081202     D PiInt           S               N   INZ
006500081202     D PiNum           S               N   INZ
006600081202
006700081202     D*------------------
006800081202     D* LINKING A DEFINIZIONI ESTERNE
006900081202     D*------------------
007000081202     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007100081202
007200010330
007300010330
007400990915     C                   time                    wrkdata
007500990915     C                   time                    wrkora
007600000913     C                   reset                   rrnum
007700990921     C                   reset                   esito
007800990921     C                   reset                   wrkesito
007900010601     C*
008000010601     C                   exsr      opeini
008100010601     C                   exsr      rwvad
008200010601     C*
008300010601     C                   seton                                        lr
008400010601
008500010601
008600010601
008700010601
008800010601     C*--------------------------------------------------------
008900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009000010601     C*--------------------------------------------------------
009100010601     C     PREELA        BEGSR
009200010601     C*
009300010601     C* SE OCCORRE SPEDIRE IN FILIALE
009400010601     C                   if        invfil <> *zeros and
009500010601     C                             flgGiro = '0'
009600010601     C*
009700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009800010601     C                   eval      flgGiro = '1'
009900000724     C*
010000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010200010601     C     invfil        chain     azorg01l
010300000724     C                   if        %found
010400000724     C                   movel(p)  CMD(1)        depcmd
010500020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010600000724     C*
010700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010800011123     C                   Z-ADD     150           LENGH            15 5
010900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011000000724     C                   PARM                    depcmd
011100000724     C                   PARM                    LENGH
011200000724     C*
011300000724     C                   endif
011400000616     C*
011500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011600000616     C                   movel(p)  CMD(2)        depcmd
011700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011800000616     C*
011900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012000011123     C                   Z-ADD     150           LENGH            15 5
012100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012200000616     C                   PARM                    depcmd
012300000616     C                   PARM                    LENGH
012400000613     C*
012500050613     C                   if        not %open(tis7tedps)
012600050613     C                   open      tis7tedps
012700030407     C   50              except    testdett
012800000613     C                   endif
012900010601     C*
013000010601     C                   endif
013100010601     C*
013200010601     C                   ENDSR
013300010601     C***
013400010601
013500010601
013600010601
013700010601
013800010601
013900010601
014000010601
014100010601     C*--------------------------------------------------------
014200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014300010601     C*--------------------------------------------------------
014400010601     C     ENDELA        BEGSR
014500990915     C*
014600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014700000613     C*
014800050613     C                   if        %open(tis7tedps)
014900030407     C   50              except    findett
015000050613     C                   close     tis7tedps
015100000613     C                   endif
015200000616     C*
015300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015400011123     C                   Z-ADD     150           LENGH            15 5
015500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015600000616     C                   PARM                    CMD(3)
015700000616     C                   PARM                    LENGH
015800000616     C*
015900010601     C                   ENDSR
016000010601     C***
016100010601
016200010601
016300010601
016400010601
016500000613
016600000613     C*--------------------------------------------------------
016700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016800000613     C*--------------------------------------------------------
016900000621     C     STPR          BEGSR
017000000613     C*
017100050613     C                   if        not %open(tis7tedp)
017200050613     C                   open      tis7tedp
017300990915     C                   endif
017400990915     C*
017500030407     C   50              except    riepilogo
017600990915     C*
017700050613     C                   if        %open(tis7tedp)
017800050613     C                   close     tis7tedp
017900990914     C                   endif
018000000613     C*
018100000613     C                   ENDSR
018200000613     C***
018300000613
018400010330
018500010601
018600010601
018700010601
018800010330     C*--------------------------------------------------------
018900050613     C* RWVAD   LEGGE tivin00r E SCRIVE EDIVADWF              *
019000010330     C*--------------------------------------------------------
019100010330     C     RWVAD         BEGSR
019200010330     C*
019300050613     C*
019400050613     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
019500050613     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
019600050613     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
019700050613     C                   if        *in60 = *on
019800010330     C                   if        not %open(tivin00r)
019900010330     C                   open      tivin00r
020000010330     C                   endif
020100050613     C                   if        not %open(edivadwr)
020200050613     C                   open      edivadwr
020300010330     C                   endif
020400010330     C*
020500010604     C                   clear                   §CTROK
020600010604     C                   clear                   §CTRMO
020700010604     C                   clear                   §CTRNO
020800010330     C*
020900010330     C                   DO        *HIVAL
021000010330     C*
021100010330     C                   READ      tivin00r                               70
021200010618     C*
021300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021400010618     C                   if        vinflg = '1'
021500010618     C                   eval      flgOk = '1'
021600010618     C                   endif
021700010618     C*
021800010330     C                   if        vindta > *blanks
021900010330     C                   add       1             rrnum
022000010330     C*
022100010601     C                   if        *in70 = *off and
022200010330     C                             (vinflg = *blanks
022300010330     C                              or vinflg = '0'
022400010330     C                              or vinflg = '2')
022500010330     C*
022600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022700010711     C                   if        vinflg = *blanks or vinflg = '0'
022800010711     C                   clear                   vinmsg
022900010711     C                   endif
023000010601     C*
023100010330     C                   exsr      inzvar
023200010330     C                   exsr      defcam
023300010330     C                   exsr      impvad
023400050613     C*
023500050613     C* VALORIZZO CAMPI RELATIVI AL "CMR"
023600050613     C                   EVAL      VADCMR = wCMR
023700050613     C                   EVAL      VADCNT = 1
023800010601     C*
023900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024000010601     C                   eval      depfil = VADLNP
024100010601     C                   exsr      repfil
024200010601     C                   if        depfil = invfil
024300021014     C                   if        vlrpoi = 999
024400021010     C                   MOVE(P)   invfil        VADFGS
024500021014     C                   else
024600021014     C                   MOVE(P)   vlrpoi        VADFGS
024700021014     C                   endif
024800010601     C*
024900010601     C                   exsr      PREELA
025000051212
025100051212      * "bonifico" i dati di input da eventuali caratteri indesiderati
025200051212     c                   exsr      exeBon
025300081202
025400081202     C*
025500081202     C* Forzatura x NSP = RMA
025600081202     C                   EVAL      PiStr=%trim(%subst(vindta:59:15))
025700081202     C                   EXSR      CHKNUM
025800081202     C                   IF        PiInt=*on
025900081202     C                   Z-ADD     PiVal         VADNSP
026000081202     C                   ELSE
026100081202     C                   SETON                                        31
026200081202     C                   EVAL      vinmsg = %trimr(vinmsg)
026300081202     C                             + ' ' + 'VADNSP/RMA'
026400081202     C                   ENDIF
026500051212
026600010601     C*
026700010604     C* Ebbene...
026800010604     C*
026900010604     C  N31              ADD       1             §CTROK            7 0
027000010604     C   32              ADD       1             §CTRMO            7 0
027100010604     C   31              ADD       1             §CTRNO            7 0
027200050613     C  N31              WRITE     EDIVAD00
027300010604     C*
027400010604     C                   if        *in31 = *off and
027500010604     C                             *in32 = *off
027600010604     C                   eval      vinflg = '1'
027700010604     C                   else
027800010604     C                   eval      recko = vindta
027900050613     C                   if        %open(tis7tedps)
028000030407     C   50              except    rigadett
028100010612     C                   endif
028200010604     C                   eval      vinflg = '2'
028300010604     C                   endif
028400010604     C                   endif
028500010601     C*
028600010604     C                   endif
028700010604     C*
028800010330     C                   else
028900010330     C                   eval      vinflg = '1'
029000010330     C                   endif
029100010601     C*
029200010601     C  N70              update    tivin000
029300010330     C*
029400010330     C  N70              ENDdo
029500010601     C
029600010601     C*
029700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029800010601     C                   if        cntNonEl = *zeros or
029900010601     C                             flgMulti = '0'
030000010330     C* Se non ci sono record con errori ...
030100010601     C                   if        §ctrno = 0 and
030200010604     C                             §ctrmo = 0 and
030300010601     C                             flgStato <> '2'
030400010330     C* ... restituisco esito OK.
030500010330     C                   eval      wrkesito = '0'
030600010330     C                   else
030700010330     C                   if        §ctrok > 0
030800010330     C                   eval      wrkesito = '1'
030900010330     C                   else
031000010615     C                   if        flgOk = '0'
031100010615     C                   eval      wrkesito = '2'
031200010615     C                   else
031300010615     C                   eval      wrkesito = '6'
031400010615     C                   endif
031500010330     C                   endif
031600010330     C                   endif
031700010601     C                   else
031800010601     C                   eval      wrkesito = '9'
031900010601     C                   endif
032000050613     C*
032100050613     C                   else
032200050613     C                   eval      wrkesito = '8'
032300050613     C                   eval      flgGiro = '1'
032400050613     C                   endif
032500010330     C*
032600010330     C                   if        %open(tivin00r)
032700010330     C                   close     tivin00r
032800010330     C                   endif
032900050613     C                   if        %open(edivadwr)
033000050613     C                   close     edivadwr
033100010330     C                   endif
033200010601     C*
033300010601     C                   if        vlrpoi <> 999
033400010601     C                   eval      invfil = vlrpoi
033500010601     C                   endif
033600010330     C*
033700010330     C                   if        §ctrok > 0
033800010601     C                             and invfil > *zeros
033900010330     C                   exsr      invio
034000010330     C                   endif
034100010612     C*
034200010612     C                   if        flgGiro = '1'
034300010612     C                   exsr      endela
034400010612     C                   endif
034500010330     C*
034600010330     C                   ENDSR
034700010330     C***
034800010330
034900010601
035000010601
035100010601
035200010601
035300010330     C*----------------------------------------------------*
035400010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
035500010330     C*----------------------------------------------------*
035600010330     C     INZVAR        BEGSR
035700010330     C*
035800010330     C*
035900010330     C                   ENDSR
036000010330     C*----------------------------------------------------*
036100010330     C*  IMPOSTAZIONE CAMPI COSTANTI
036200010330     C*----------------------------------------------------*
036300010330     C     DEFCAM        BEGSR
036400010330     C*
036500050613     C                   CLEAR                   EDIVAD00
036600010330     C*
036700010330     C                   ENDSR
036800010330     C*----------------------------------------------------*
036900050613     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAD)
037000010330     C*----------------------------------------------------*
037100010330     C     IMPVAD        BEGSR
037200010330     C*
037300010604     C                   SETOFF                                       3132
037400010330     C*
037500010330     C* Reperimento campi ALFA
037600010330     C                   EVAL      VADATB=%trim(%subst(vindta:1:1))
037700010330     C                   EVAL      VADCDP=%trim(%subst(vindta:25:15))
037800010330     C*
037900010330     C                   EVAL      PiStr=%trim(%subst(vindta:2:7))
038000010330     C                   EXSR      CHKNUM
038100010330     C                   IF        PiInt=*on
038200010330     C                   Z-ADD     PiVal         VADCCM
038300010330     C                   ELSE
038400010604     C                   SETON                                        31
038500010330     C                   EVAL      vinmsg = %trimr(vinmsg)
038600010330     C                             + ' ' + 'VADCCM'
038700010330     C                   ENDIF
038800010330     C*
038900010330     C                   EVAL      PiStr=%trim(%subst(vindta:9:3))
039000010330     C                   EXSR      CHKNUM
039100010330     C                   IF        PiInt=*on
039200010330     C                   Z-ADD     PiVal         VADLNP
039300010330     C                   ELSE
039400010604     C                   SETON                                        31
039500010330     C                   EVAL      vinmsg = %trimr(vinmsg)
039600021010     C                             + ' ' + 'VADLNP'
039700010330     C                   ENDIF
039800050104     C*
039900050104     C                   IF        %subst(vindta:12:4) <> *blanks
040000050104     C                   EVAL      PiStr=%trim(%subst(vindta:12:4))
040100050104     C                   EXSR      CHKNUM
040200050104     C                   IF        PiInt=*on
040300050104     C                   Z-ADD     PiVal         VADAAS
040400050104     C                   ELSE
040500050104     C                   SETON                                        31
040600050104     C                   EVAL      vinmsg = %trimr(vinmsg)
040700050104     C                             + ' ' + 'VADAAS'
040800050104     C                   ENDIF
040900050104     C                   ELSE
041000050104     C                   MOVEL     datcor        VADAAS
041100050104     C                   ENDIF
041200010330     C*
041300010330     C                   EVAL      PiStr=%trim(%subst(vindta:16:2))
041400010330     C                   EXSR      CHKNUM
041500010330     C                   IF        PiInt=*on
041600010330     C                   Z-ADD     PiVal         VADNRS
041700010330     C                   ELSE
041800010604     C                   SETON                                        32
041900010604     C                   Z-ADD     *zeros        VADNRS
042000010330     C                   EVAL      vinmsg = %trimr(vinmsg)
042100010330     C                             + ' ' + 'VADNRS'
042200010330     C                   ENDIF
042300010330     C*
042400010330     C                   EVAL      PiStr=%trim(%subst(vindta:18:7))
042500010330     C                   EXSR      CHKNUM
042600010330     C                   IF        PiInt=*on
042700010330     C                   Z-ADD     PiVal         VADNSP
042800010330     C                   ELSE
042900010604     C                   SETON                                        32
043000010604     C                   Z-ADD     *zeros        VADNSP
043100010330     C                   EVAL      vinmsg = %trimr(vinmsg)
043200010330     C                             + ' ' + 'VADNSP'
043300010330     C                   ENDIF
043400010330     C*
043500010330     C                   EVAL      PiStr=%trim(%subst(vindta:40:5))
043600010330     C                   EXSR      CHKNUM
043700010330     C                   IF        PiInt=*on
043800010330     C                   Z-ADD     PiVal         VADNCL
043900010330     C                   ELSE
044000010604     C                   SETON                                        32
044100010604     C                   Z-ADD     *zeros        VADNCL
044200010330     C                   EVAL      vinmsg = %trimr(vinmsg)
044300010330     C                             + ' ' + 'VADNCL'
044400010330     C                   ENDIF
044500010330     C*
044600010330     C                   EVAL      PiStr=%trim(%subst(vindta:45:7))
044700010330     C                   EXSR      CHKNUM
044800010330     C                   IF        PiInt=*on
044900010330     C                   Z-ADD     PiVal         VADNCD
045000010330     C                   ELSE
045100010604     C                   SETON                                        32
045200010604     C                   Z-ADD     *zeros        VADNCD
045300010330     C                   EVAL      vinmsg = %trimr(vinmsg)
045400010330     C                             + ' ' + 'VADNCD'
045500010330     C                   ENDIF
045600010330     C*
045700010330     C                   EVAL      PiStr=%trim(%subst(vindta:52:7))
045800010330     C                   EXSR      CHKNUM
045900010330     C                   IF        PiInt=*on
046000010330     C                   Z-ADD     PiVal         VADNCA
046100010330     C                   ELSE
046200010604     C                   SETON                                        32
046300010604     C                   Z-ADD     *zeros        VADNCA
046400010330     C                   EVAL      vinmsg = %trimr(vinmsg)
046500010330     C                             + ' ' + 'VADNCA'
046600010330     C                   ENDIF
046700010330     C*
046800010330     C                   ENDSR
046900010601
047000010601
047100010601
047200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
047300010601     C     repfil        BEGSR
047400010601     C*
047500010601     C                   if        invfil = *zeros and
047600010601     C                             depfil > *zeros and
047700010613     C                             (vinflg = *blanks or
047800010613     C                              vinflg = *zeros)
047900010601     C
048000010601     C                   eval      invfil = depfil
048100010601     C                   endif
048200010601     C*
048300010601     C                   if        depfil <> invfil and
048400010601     C                             invfil > *zeros
048500010601     C                   eval      flgMulti = '1'
048600010601     C                   if        vinflg = *blanks
048700010601     C                   add       1             cntNonEl
048800010601     C                   endif
048900010601     C                   endif
049000010601     C*
049100010601     C                   if        vinflg = '2'
049200010601     C                   eval      flgStato = '2'
049300010601     C                   endif
049400010601     C*
049500010601     C                   ENDSR
049600010601     C***
049700010601
049800010601
049900010330
050000010330
050100010330
050200990920      /TITLE Invio dei dati al punto operativo.
050300000613     C     invio         BEGSR
050400990920     C*
050500990920     C                   reset                   dscmz
050600010601     C                   move      invfil        cmzdst
050700990920     C                   eval      cmzfld = vlrfou
050800990920     C                   eval      cmzmbd = vlrhdl
050900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
051000000710     C                   if        prmfir = *blanks
051100050613     C                   eval      cmzfla = 'EDIVAD0F'
051200050613     C                   eval      cmzmba = 'EDIVAD0F'
051300000710     C                   else
051400000710     C                   eval      cmzfla = prmfir
051500000710     C                   eval      cmzmba = prmfir
051600000710     C                   endif
051700990920     C                   eval      cmznrr = *zeros
051800990920     C                   move      §ctrok        cmznrr
051900021003     C                   eval      cmzlba = vlrfl1
052000990920     C                   call(e)   'TIS711C'
052100990920     C                   parm                    dscmz
052200990921     C                   parm      *blanks       esito
052300990920     C                   if        %error
052400990920     C                             or cmzerr = '1'
052500990921     C                             or esito  = '1'
052600000710     C                   eval      wrkesito = '3'
052700990920     C                   endif
052800990920     C*
052900000613     C                   ENDSR
053000990910
053100010601
053200010601
053300010601
053400010601
053500010601      /TITLE Invio dei dati al punto operativo.
053600010601     C     opeini        BEGSR
053700010601     C*
053800010601     C* Inizializzo flag e contatori operativi
053900010601     C                   movel     '0'           flgGiro           1
054000010601     C                   movel     '0'           flgMulti          1
054100010601     C                   movel     '1'           flgStato          1
054200010615     C                   movel     '0'           flgOk             1
054300010601     C                   z-add     *zeros        cntNonEl         10 0
054400010601     C                   z-add     *zeros        depfil            3 0
054500010601     C                   z-add     *zeros        invfil            3 0
054600030407     C*
054700030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
054800030407     C                   if        %subst(vlrppt:1:1) = 'S'
054900030407     C                   seton                                        50
055000030407     C                   else
055100030407     C                   setoff                                       50
055200030407     C                   endif
055300010601     C*
055400010601     C                   ENDSR
055500010601     C***
055600010601
055700051212
055800051212
055900051212      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
056000051212     C     exeBon        BEGSR
056100051212     C*
056200051212     C* Imposto i parametri x il *pgm d "bonifica"
056300051212     C                   EVAL      ElencoChar = *blanks
056400051212     C                   EVAL      TipoElenco = *blanks
056500051212     C                   EVAL      CharSost   = *blanks
056600051212     C                   EVAL      UpperCase  = '1'
056700051212     C                   EVAL      ChkNull    = '1'
056800051212     C                   EVAL      CharNull   = *blanks
056900051212     C*
057000051212     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VADCDP
057100051212     C                   EVAL      TxtInOut   = VADCDP
057200051212     C                   EVAL      EsitoBon   = *blanks
057300051212     C*
057400051212     C                   CALL      'XCHKCHAR'
057500051212     C                   PARM                    TxtInOut
057600051212     C                   PARM                    ElencoChar
057700051212     C                   PARM                    TipoElenco
057800051212     C                   PARM                    CharSost
057900051212     C                   PARM                    UpperCase
058000051212     C                   PARM                    ChkNull
058100051212     C                   PARM                    CharNull
058200051212     C                   PARM                    EsitoBon
058300051212     C*
058400051212     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
058500051212     C                   IF        EsitoBon = '1'
058600051212     C                   EVAL      VADCDP = TxtInOut
058700051212     C                   ENDIF
058800051212     C*
058900051212     C                   ENDSR
059000010601
059100081202
059200081202
059300081202     C*----------------------------------------------------*
059400081202     C*  CONTROLLO NUMERICITA' CAMPI
059500081202     C*----------------------------------------------------*
059600081202     C     CHKNUM        BEGSR
059700081202     C*
059800081202     C                   IF        PiDecChr = *blanks
059900081202     C                   EVAL      PiDecChr = '.'
060000081202     C                   ENDIF
060100081202     C*
060200081202     C                   callp(e)  UBISNUM_Check(PiStr
060300081202     C                                          :PiDecChr
060400081202     C                                          :PiVal
060500081202     C                                          :PiNum
060600081202     C                                          :PiInt)
060700081202     C*
060800081202     C                   IF        %error
060900081202     C                   EVAL      PiInt=*off
061000081202     C                   ENDIF
061100081202     C*
061200081202     C                   ENDSR
061300081202     C***
061400081202
061500010330
061600010330
061700000613     C     *inzsr        BEGSR
061800990910     C*
061900990910     C     *entry        plist
062000990920     C                   parm                    tivlrds
062100990921     C                   parm      wrkesito      esito
062200000724     C                   parm                    prmlit
062300000710     C                   parm                    prmfir
062400050613     C*
062500050613     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
062600050613     C                   move(p)   vlrMSG        wNomeFile
062700050613     C*
062800050613     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
062900050613     C* del codice CMR dal nome del file importato.
063000050613     C                   if        wNomeFile <> *blanks AND
063100050613     C                             vlrppt<>*blanks
063200050613     C                   if        %subst(vlrppt:1:1) = 'S'
063300050613     C                   seton                                        50
063400050613     C                   else
063500050613     C                   setoff                                       50
063600050613     C                   endif
063700081202     C                   if        %subst(vlrppt:2:4) > *zeros
063800050613     C                   eval      wPosDaA = %subst(vlrppt:2:2)
063900050613     C                   move(p)   wPosDaA       wPosDa
064000050613     C                   eval      wLungA  = %subst(vlrppt:4:2)
064100050613     C                   move(p)   wLungA        wLung
064200050613     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
064300081202     C                   else
064400081202     C                   eval      wCMR = 'CEVA - TIM'
064500081202     C                   endif
064600050613     C                   seton                                        60
064700050613     C                   else
064800050613     C                   setoff                                       60
064900050613     C                   endif
065000010330     C*
065100010330     C* CALCOLA LA DATA CORRENTE
065200010330     C                   time                    wn14             14 0
065300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
065400010330     C                   z-add     wn8           g08dat
065500010330     C                   z-add     *zeros        g08inv
065600010330     C                   movel     '0'           g08err
065700010330     C                   call      'XSRDA8'
065800010330     C                   parm                    wlbda8
065900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
066000000613     C*
066100000613     C                   ENDSR
066200000613     C***
066300990908
066400050613     Otis7tedp  E            riepilogo         2
066500990915     O                                              'Upload via Internet'
066600990915     O                                           +1 'Traduzione TIVIN00R -'
066700050613     O                                           +1 'EDIVADWR'
066800990915     O                                           +1 'Testate bolle'
066900990915     O          E            riepilogo   2
067000990915     O                       wrkdata
067100990915     O                       wrkora              +1
067200990915     O                       procname            +1
067300990915     O          E            riepilogo   2
067400990915     O                                              'Cliente..................:'
067500010330     O                       VADCCM        z     +1
067600990915     O          E            riepilogo   2
067700990920     O                                              'Riferimento Strategi.....:'
067800990920     O                       vlrhdl              +1
067900990915     O          E            riepilogo   2
068000990915     O                                              'Giusti...................:'
068100971022     O                       §CTROK        2   +  1
068200990915     O          E            riepilogo   2
068300990915     O                                              'Sbagliati e corretti.....:'
068400971022     O                       §CTRMO        2   +  1
068500990915     O          E            riepilogo   2
068600990915     O                                              'Sbagliati e scartati.....:'
068700971022     O                       §CTRNO        2   +  1
068800000613
068900050613     Otis7tedps E            testdett          2
069000000613     O                                              'Upload via Internet'
069100000613     O                                           +1 'Traduzione TIVIN00R -'
069200050613     O                                           +1 'EDIVADWR'
069300000613     O                                           +1 'Testate bolle'
069400000616     O          E            testdett    3
069500000613     O                                           +2 'N° rec'
069600000613     O                                           +3 'Anteprima contenuto'
069700000616     O          E            rigadett    2
069800000613     O                       rrnum               +2
069900000621     O                       recko               +3
070000000616     O          E            findett     2
070100000613     O                       wrkdata
070200000613     O                       wrkora              +1
070300000613     O                       procname            +1
070400000616     O          E            findett     2
070500000613     O                                              'Cliente..................:'
070600010330     O                       VADCCM        z     +1
070700000616     O          E            findett     2
070800000613     O                                              'Riferimento Strategi.....:'
070900000613     O                       vlrhdl              +1
071000000616     O          E            findett     2
071100000613     O                                              'Giusti...................:'
071200000613     O                       §CTROK        2   +  1
071300000616     O          E            findett     2
071400000613     O                                              'Sbagliati e corretti.....:'
071500000613     O                       §CTRMO        2   +  1
071600000616     O          E            findett     2
071700000613     O                                              'Sbagliati e scartati.....:'
071800000613     O                       §CTRNO        2   +  1
071900000613** CMD - COMANDI CL
072000060403OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7TEDP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
072100060403OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7TEDPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
072200050613DLTOVR FILE(TIS7TEDP TIS7TEDPS) LVL(*)
072300000613
072400000613
