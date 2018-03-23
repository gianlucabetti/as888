000100020916      /TITLE Upload via Internet: traduzione in FIVADWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600020916     FFIVADwwr  O    E             DISK    usropn
000700010330     Ftis7t8p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900010330     Ftis7t8ps  O    f  198        PRINTER usropn
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
003200010330     D*------------------
003300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003400010330     D*------------------
003500010330     D WLBDA8          DS                  INZ
003600010330     D  G08DAT                 1      8  0
003700010330     D  G08INV                 9     16  0
003800010330     D  G08ERR                17     17
003900010330     D  G08TGI                18     22  0
004000000313     D*
004100010330
004200051212
004300051212     D TxtInOut        S           2048
004400051212     D ElencoChar      S            256
004500051212     D TipoElenco      S              1
004600051212     D CharSost        S              1
004700051212     D UpperCase       S              1
004800051212     D ChkNull         S              1
004900051212     D CharNull        S              1
005000051212     D EsitoBon        S              1
005100010330
005200010330
005300990915     C                   time                    wrkdata
005400990915     C                   time                    wrkora
005500000913     C                   reset                   rrnum
005600990921     C                   reset                   esito
005700990921     C                   reset                   wrkesito
005800010601     C*
005900010601     C                   exsr      opeini
006000010601     C                   exsr      rwvad
006100010601     C*
006200010601     C                   seton                                        lr
006300010601
006400010601
006500010601
006600010601
006700010601     C*--------------------------------------------------------
006800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006900010601     C*--------------------------------------------------------
007000010601     C     PREELA        BEGSR
007100010601     C*
007200010601     C* SE OCCORRE SPEDIRE IN FILIALE
007300010601     C                   if        invfil <> *zeros and
007400010601     C                             flgGiro = '0'
007500010601     C*
007600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007700010601     C                   eval      flgGiro = '1'
007800000724     C*
007900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008100010601     C     invfil        chain     azorg01l
008200000724     C                   if        %found
008300000724     C                   movel(p)  CMD(1)        depcmd
008400020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008500000724     C*
008600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008700011123     C                   Z-ADD     150           LENGH            15 5
008800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008900000724     C                   PARM                    depcmd
009000000724     C                   PARM                    LENGH
009100000724     C*
009200000724     C                   endif
009300000616     C*
009400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009500000616     C                   movel(p)  CMD(2)        depcmd
009600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009700000616     C*
009800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009900011123     C                   Z-ADD     150           LENGH            15 5
010000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010100000616     C                   PARM                    depcmd
010200000616     C                   PARM                    LENGH
010300000613     C*
010400010330     C                   if        not %open(tis7t8ps)
010500010330     C                   open      tis7t8ps
010600030407     C   50              except    testdett
010700000613     C                   endif
010800010601     C*
010900010601     C                   endif
011000010601     C*
011100010601     C                   ENDSR
011200010601     C***
011300010601
011400010601
011500010601
011600010601
011700010601
011800010601
011900010601
012000010601     C*--------------------------------------------------------
012100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012200010601     C*--------------------------------------------------------
012300010601     C     ENDELA        BEGSR
012400990915     C*
012500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012600000613     C*
012700010330     C                   if        %open(tis7t8ps)
012800030407     C   50              except    findett
012900010330     C                   close     tis7t8ps
013000000613     C                   endif
013100000616     C*
013200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013300011123     C                   Z-ADD     150           LENGH            15 5
013400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013500000616     C                   PARM                    CMD(3)
013600000616     C                   PARM                    LENGH
013700000616     C*
013800010601     C                   ENDSR
013900010601     C***
014000010601
014100010601
014200010601
014300010601
014400000613
014500000613     C*--------------------------------------------------------
014600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014700000613     C*--------------------------------------------------------
014800000621     C     STPR          BEGSR
014900000613     C*
015000010330     C                   if        not %open(tis7t8p)
015100010330     C                   open      tis7t8p
015200990915     C                   endif
015300990915     C*
015400030407     C   50              except    riepilogo
015500990915     C*
015600010330     C                   if        %open(tis7t8p)
015700010330     C                   close     tis7t8p
015800990914     C                   endif
015900000613     C*
016000000613     C                   ENDSR
016100000613     C***
016200000613
016300010330
016400010601
016500010601
016600010601
016700010330     C*--------------------------------------------------------
016800020916     C* RWVAD   LEGGE tivin00r E SCRIVE FIVADWWF              *
016900010330     C*--------------------------------------------------------
017000010330     C     RWVAD         BEGSR
017100010330     C*
017200010330     C                   if        not %open(tivin00r)
017300010330     C                   open      tivin00r
017400010330     C                   endif
017500020916     C                   if        not %open(fivadwwr)
017600020916     C                   open      fivadwwr
017700010330     C                   endif
017800010330     C*
017900010604     C                   clear                   §CTROK
018000010604     C                   clear                   §CTRMO
018100010604     C                   clear                   §CTRNO
018200010330     C*
018300010330     C                   DO        *HIVAL
018400010330     C*
018500010330     C                   READ      tivin00r                               70
018600010618     C*
018700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018800010618     C                   if        vinflg = '1'
018900010618     C                   eval      flgOk = '1'
019000010618     C                   endif
019100010618     C*
019200010330     C                   if        vindta > *blanks
019300010330     C                   add       1             rrnum
019400010330     C*
019500010601     C                   if        *in70 = *off and
019600010330     C                             (vinflg = *blanks
019700010330     C                              or vinflg = '0'
019800010330     C                              or vinflg = '2')
019900010330     C*
020000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020100010711     C                   if        vinflg = *blanks or vinflg = '0'
020200010711     C                   clear                   vinmsg
020300010711     C                   endif
020400010601     C*
020500010330     C                   exsr      inzvar
020600010330     C                   exsr      defcam
020700010330     C                   exsr      impvad
020800010601     C*
020900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021000010601     C                   eval      depfil = VADLNP
021100010601     C                   exsr      repfil
021200010601     C                   if        depfil = invfil
021300021014     C                   if        vlrpoi = 999
021400021010     C                   MOVE(P)   invfil        VADFGS
021500021014     C                   else
021600021014     C                   MOVE(P)   vlrpoi        VADFGS
021700021014     C                   endif
021800010601     C*
021900010601     C                   exsr      PREELA
022000051212
022100051212      * "bonifico" i dati di input da eventuali caratteri indesiderati
022200051212     c                   exsr      exeBon
022300051212
022400010601     C*
022500010604     C* Ebbene...
022600010604     C*
022700010604     C  N31              ADD       1             §CTROK            7 0
022800010604     C   32              ADD       1             §CTRMO            7 0
022900010604     C   31              ADD       1             §CTRNO            7 0
023000020916     C  N31              WRITE     FIVAD000
023100010604     C*
023200010604     C                   if        *in31 = *off and
023300010604     C                             *in32 = *off
023400010604     C                   eval      vinflg = '1'
023500010604     C                   else
023600010604     C                   eval      recko = vindta
023700010612     C                   if        %open(tis7t8ps)
023800030407     C   50              except    rigadett
023900010612     C                   endif
024000010604     C                   eval      vinflg = '2'
024100010604     C                   endif
024200010604     C                   endif
024300010601     C*
024400010604     C                   endif
024500010604     C*
024600010330     C                   else
024700010330     C                   eval      vinflg = '1'
024800010330     C                   endif
024900010601     C*
025000010601     C  N70              update    tivin000
025100010330     C*
025200010330     C  N70              ENDdo
025300010601     C
025400010601     C*
025500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025600010601     C                   if        cntNonEl = *zeros or
025700010601     C                             flgMulti = '0'
025800010330     C* Se non ci sono record con errori ...
025900010601     C                   if        §ctrno = 0 and
026000010604     C                             §ctrmo = 0 and
026100010601     C                             flgStato <> '2'
026200010330     C* ... restituisco esito OK.
026300010330     C                   eval      wrkesito = '0'
026400010330     C                   else
026500010330     C                   if        §ctrok > 0
026600010330     C                   eval      wrkesito = '1'
026700010330     C                   else
026800010615     C                   if        flgOk = '0'
026900010615     C                   eval      wrkesito = '2'
027000010615     C                   else
027100010615     C                   eval      wrkesito = '6'
027200010615     C                   endif
027300010330     C                   endif
027400010330     C                   endif
027500010601     C                   else
027600010601     C                   eval      wrkesito = '9'
027700010601     C                   endif
027800010330     C*
027900010330     C                   if        %open(tivin00r)
028000010330     C                   close     tivin00r
028100010330     C                   endif
028200020916     C                   if        %open(fivadwwr)
028300020916     C                   close     fivadwwr
028400010330     C                   endif
028500010601     C*
028600010601     C                   if        vlrpoi <> 999
028700010601     C                   eval      invfil = vlrpoi
028800010601     C                   endif
028900010330     C*
029000010330     C                   if        §ctrok > 0
029100010601     C                             and invfil > *zeros
029200010330     C                   exsr      invio
029300010330     C                   endif
029400010612     C*
029500010612     C                   if        flgGiro = '1'
029600010612     C                   exsr      endela
029700010612     C                   endif
029800010330     C*
029900010330     C                   ENDSR
030000010330     C***
030100010330
030200010601
030300010601
030400010601
030500010601
030600010330     C*----------------------------------------------------*
030700010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030800010330     C*----------------------------------------------------*
030900010330     C     INZVAR        BEGSR
031000010330     C*
031100010330     C*
031200010330     C                   ENDSR
031300010330     C*----------------------------------------------------*
031400010330     C*  IMPOSTAZIONE CAMPI COSTANTI
031500010330     C*----------------------------------------------------*
031600010330     C     DEFCAM        BEGSR
031700010330     C*
031800020916     C                   CLEAR                   FIVAD000
031900010330     C*
032000010330     C                   ENDSR
032100010330     C*----------------------------------------------------*
032200020916     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAD)
032300010330     C*----------------------------------------------------*
032400010330     C     IMPVAD        BEGSR
032500010330     C*
032600010604     C                   SETOFF                                       3132
032700010330     C*
032800010330     C* Reperimento campi ALFA
032900010330     C                   EVAL      VADATB=%trim(%subst(vindta:1:1))
033000010330     C                   EVAL      VADCDP=%trim(%subst(vindta:25:15))
033100010330     C*
033200010330     C                   EVAL      PiStr=%trim(%subst(vindta:2:7))
033300010330     C                   EXSR      CHKNUM
033400010330     C                   IF        PiInt=*on
033500010330     C                   Z-ADD     PiVal         VADCCM
033600010330     C                   ELSE
033700010604     C                   SETON                                        31
033800010330     C                   EVAL      vinmsg = %trimr(vinmsg)
033900010330     C                             + ' ' + 'VADCCM'
034000010330     C                   ENDIF
034100010330     C*
034200010330     C                   EVAL      PiStr=%trim(%subst(vindta:9:3))
034300010330     C                   EXSR      CHKNUM
034400010330     C                   IF        PiInt=*on
034500010330     C                   Z-ADD     PiVal         VADLNP
034600010330     C                   ELSE
034700010604     C                   SETON                                        31
034800010330     C                   EVAL      vinmsg = %trimr(vinmsg)
034900021010     C                             + ' ' + 'VADLNP'
035000010330     C                   ENDIF
035100050104     C*
035200050104     C                   IF        %subst(vindta:12:4) <> *blanks
035300050104     C                   EVAL      PiStr=%trim(%subst(vindta:12:4))
035400050104     C                   EXSR      CHKNUM
035500050104     C                   IF        PiInt=*on
035600050104     C                   Z-ADD     PiVal         VADAAS
035700050104     C                   ELSE
035800050104     C                   SETON                                        31
035900050104     C                   EVAL      vinmsg = %trimr(vinmsg)
036000050104     C                             + ' ' + 'VADAAS'
036100050104     C                   ENDIF
036200050104     C                   ELSE
036300050104     C                   MOVEL     datcor        VADAAS
036400050104     C                   ENDIF
036500010330     C*
036600010330     C                   EVAL      PiStr=%trim(%subst(vindta:16:2))
036700010330     C                   EXSR      CHKNUM
036800010330     C                   IF        PiInt=*on
036900010330     C                   Z-ADD     PiVal         VADNRS
037000010330     C                   ELSE
037100010604     C                   SETON                                        32
037200010604     C                   Z-ADD     *zeros        VADNRS
037300010330     C                   EVAL      vinmsg = %trimr(vinmsg)
037400010330     C                             + ' ' + 'VADNRS'
037500010330     C                   ENDIF
037600010330     C*
037700010330     C                   EVAL      PiStr=%trim(%subst(vindta:18:7))
037800010330     C                   EXSR      CHKNUM
037900010330     C                   IF        PiInt=*on
038000010330     C                   Z-ADD     PiVal         VADNSP
038100010330     C                   ELSE
038200010604     C                   SETON                                        32
038300010604     C                   Z-ADD     *zeros        VADNSP
038400010330     C                   EVAL      vinmsg = %trimr(vinmsg)
038500010330     C                             + ' ' + 'VADNSP'
038600010330     C                   ENDIF
038700010330     C*
038800010330     C                   EVAL      PiStr=%trim(%subst(vindta:40:5))
038900010330     C                   EXSR      CHKNUM
039000010330     C                   IF        PiInt=*on
039100010330     C                   Z-ADD     PiVal         VADNCL
039200010330     C                   ELSE
039300010604     C                   SETON                                        32
039400010604     C                   Z-ADD     *zeros        VADNCL
039500010330     C                   EVAL      vinmsg = %trimr(vinmsg)
039600010330     C                             + ' ' + 'VADNCL'
039700010330     C                   ENDIF
039800010330     C*
039900010330     C                   EVAL      PiStr=%trim(%subst(vindta:45:7))
040000010330     C                   EXSR      CHKNUM
040100010330     C                   IF        PiInt=*on
040200010330     C                   Z-ADD     PiVal         VADNCD
040300010330     C                   ELSE
040400010604     C                   SETON                                        32
040500010604     C                   Z-ADD     *zeros        VADNCD
040600010330     C                   EVAL      vinmsg = %trimr(vinmsg)
040700010330     C                             + ' ' + 'VADNCD'
040800010330     C                   ENDIF
040900010330     C*
041000010330     C                   EVAL      PiStr=%trim(%subst(vindta:52:7))
041100010330     C                   EXSR      CHKNUM
041200010330     C                   IF        PiInt=*on
041300010330     C                   Z-ADD     PiVal         VADNCA
041400010330     C                   ELSE
041500010604     C                   SETON                                        32
041600010604     C                   Z-ADD     *zeros        VADNCA
041700010330     C                   EVAL      vinmsg = %trimr(vinmsg)
041800010330     C                             + ' ' + 'VADNCA'
041900010330     C                   ENDIF
042000010330     C*
042100010330     C                   ENDSR
042200990920
042300010601
042400010601
042500010330
042600010330
042700010330     C*----------------------------------------------------*
042800010330     C*  CONTROLLO NUMERICITA' CAMPI
042900010330     C*----------------------------------------------------*
043000010330     C     CHKNUM        BEGSR
043100010330     C*
043200010330     C                   call(e)   'ISNUMERIC'
043300010330     C                   PARM                    PiStr            30
043400010330     C                   PARM      '.'           PiDecChr          1
043500010330     C                   PARM      *ZEROS        PiVal            30 9
043600010330     C                   PARM      '0'           PiInt             1
043700010330     C                   PARM      '0'           PiNum             1
043800010330     C                   IF        %error
043900010330     C                   EVAL      PiInt=*off
044000010330     C                   ENDIF
044100010330     C*
044200010330     C                   ENDSR
044300010330     C***
044400010330
044500010601
044600010601
044700010601
044800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
044900010601     C     repfil        BEGSR
045000010601     C*
045100010601     C                   if        invfil = *zeros and
045200010601     C                             depfil > *zeros and
045300010613     C                             (vinflg = *blanks or
045400010613     C                              vinflg = *zeros)
045500010601     C
045600010601     C                   eval      invfil = depfil
045700010601     C                   endif
045800010601     C*
045900010601     C                   if        depfil <> invfil and
046000010601     C                             invfil > *zeros
046100010601     C                   eval      flgMulti = '1'
046200010601     C                   if        vinflg = *blanks
046300010601     C                   add       1             cntNonEl
046400010601     C                   endif
046500010601     C                   endif
046600010601     C*
046700010601     C                   if        vinflg = '2'
046800010601     C                   eval      flgStato = '2'
046900010601     C                   endif
047000010601     C*
047100010601     C                   ENDSR
047200010601     C***
047300010601
047400010601
047500010601
047600010330
047700010330
047800010330
047900990920      /TITLE Invio dei dati al punto operativo.
048000000613     C     invio         BEGSR
048100990920     C*
048200990920     C                   reset                   dscmz
048300010601     C                   move      invfil        cmzdst
048400990920     C                   eval      cmzfld = vlrfou
048500990920     C                   eval      cmzmbd = vlrhdl
048600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
048700000710     C                   if        prmfir = *blanks
048800020916     C                   eval      cmzfla = 'FIVAD00F'
048900020916     C                   eval      cmzmba = 'FIVAD00F'
049000000710     C                   else
049100000710     C                   eval      cmzfla = prmfir
049200000710     C                   eval      cmzmba = prmfir
049300000710     C                   endif
049400990920     C                   eval      cmznrr = *zeros
049500990920     C                   move      §ctrok        cmznrr
049600021003     C                   eval      cmzlba = vlrfl1
049700990920     C                   call(e)   'TIS711C'
049800990920     C                   parm                    dscmz
049900990921     C                   parm      *blanks       esito
050000990920     C                   if        %error
050100990920     C                             or cmzerr = '1'
050200990921     C                             or esito  = '1'
050300000710     C                   eval      wrkesito = '3'
050400990920     C                   endif
050500990920     C*
050600000613     C                   ENDSR
050700990910
050800010601
050900010601
051000010601
051100010601
051200010601      /TITLE Invio dei dati al punto operativo.
051300010601     C     opeini        BEGSR
051400010601     C*
051500010601     C* Inizializzo flag e contatori operativi
051600010601     C                   movel     '0'           flgGiro           1
051700010601     C                   movel     '0'           flgMulti          1
051800010601     C                   movel     '1'           flgStato          1
051900010615     C                   movel     '0'           flgOk             1
052000010601     C                   z-add     *zeros        cntNonEl         10 0
052100010601     C                   z-add     *zeros        depfil            3 0
052200010601     C                   z-add     *zeros        invfil            3 0
052300030407     C*
052400030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
052500030407     C                   if        %subst(vlrppt:1:1) = 'S'
052600030407     C                   seton                                        50
052700030407     C                   else
052800030407     C                   setoff                                       50
052900030407     C                   endif
053000010601     C*
053100010601     C                   ENDSR
053200010601     C***
053300010601
053400051212
053500051212
053600051212      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
053700051212     C     exeBon        BEGSR
053800051212     C*
053900051212     C* Imposto i parametri x il *pgm d "bonifica"
054000051212     C                   EVAL      ElencoChar = *blanks
054100051212     C                   EVAL      TipoElenco = *blanks
054200051212     C                   EVAL      CharSost   = *blanks
054300051212     C                   EVAL      UpperCase  = '1'
054400051212     C                   EVAL      ChkNull    = '1'
054500051212     C                   EVAL      CharNull   = *blanks
054600051212     C*
054700051212     C* Effettuo la chiamata al *pgm d "bonifica" x il campo VADCDP
054800051212     C                   EVAL      TxtInOut   = VADCDP
054900051212     C                   EVAL      EsitoBon   = *blanks
055000051212     C*
055100051212     C                   CALL      'XCHKCHAR'
055200051212     C                   PARM                    TxtInOut
055300051212     C                   PARM                    ElencoChar
055400051212     C                   PARM                    TipoElenco
055500051212     C                   PARM                    CharSost
055600051212     C                   PARM                    UpperCase
055700051212     C                   PARM                    ChkNull
055800051212     C                   PARM                    CharNull
055900051212     C                   PARM                    EsitoBon
056000051212     C*
056100051212     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
056200051212     C                   IF        EsitoBon = '1'
056300051212     C                   EVAL      VADCDP = TxtInOut
056400051212     C                   ENDIF
056500051212     C*
056600051212     C                   ENDSR
056700051212
056800010601
056900010601
057000010330
057100010330
057200000613     C     *inzsr        BEGSR
057300990910     C*
057400990910     C     *entry        plist
057500990920     C                   parm                    tivlrds
057600990921     C                   parm      wrkesito      esito
057700000724     C                   parm                    prmlit
057800000710     C                   parm                    prmfir
057900010330     C*
058000010330     C* CALCOLA LA DATA CORRENTE
058100010330     C                   time                    wn14             14 0
058200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
058300010330     C                   z-add     wn8           g08dat
058400010330     C                   z-add     *zeros        g08inv
058500010330     C                   movel     '0'           g08err
058600010330     C                   call      'XSRDA8'
058700010330     C                   parm                    wlbda8
058800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
058900000613     C*
059000000613     C                   ENDSR
059100000613     C***
059200990908
059300010330     Otis7t8p   E            riepilogo         2
059400990915     O                                              'Upload via Internet'
059500990915     O                                           +1 'Traduzione TIVIN00R -'
059600020916     O                                           +1 'FIVADWWR'
059700990915     O                                           +1 'Testate bolle'
059800990915     O          E            riepilogo   2
059900990915     O                       wrkdata
060000990915     O                       wrkora              +1
060100990915     O                       procname            +1
060200990915     O          E            riepilogo   2
060300990915     O                                              'Cliente..................:'
060400010330     O                       VADCCM        z     +1
060500990915     O          E            riepilogo   2
060600990920     O                                              'Riferimento Strategi.....:'
060700990920     O                       vlrhdl              +1
060800990915     O          E            riepilogo   2
060900990915     O                                              'Giusti...................:'
061000971022     O                       §CTROK        2   +  1
061100990915     O          E            riepilogo   2
061200990915     O                                              'Sbagliati e corretti.....:'
061300971022     O                       §CTRMO        2   +  1
061400990915     O          E            riepilogo   2
061500990915     O                                              'Sbagliati e scartati.....:'
061600971022     O                       §CTRNO        2   +  1
061700000613
061800010330     Otis7t8ps  E            testdett          2
061900000613     O                                              'Upload via Internet'
062000000613     O                                           +1 'Traduzione TIVIN00R -'
062100020916     O                                           +1 'FIVADWWR'
062200000613     O                                           +1 'Testate bolle'
062300000616     O          E            testdett    3
062400000613     O                                           +2 'N° rec'
062500000613     O                                           +3 'Anteprima contenuto'
062600000616     O          E            rigadett    2
062700000613     O                       rrnum               +2
062800000621     O                       recko               +3
062900000616     O          E            findett     2
063000000613     O                       wrkdata
063100000613     O                       wrkora              +1
063200000613     O                       procname            +1
063300000616     O          E            findett     2
063400000613     O                                              'Cliente..................:'
063500010330     O                       VADCCM        z     +1
063600000616     O          E            findett     2
063700000613     O                                              'Riferimento Strategi.....:'
063800000613     O                       vlrhdl              +1
063900000616     O          E            findett     2
064000000613     O                                              'Giusti...................:'
064100000613     O                       §CTROK        2   +  1
064200000616     O          E            findett     2
064300000613     O                                              'Sbagliati e corretti.....:'
064400000613     O                       §CTRMO        2   +  1
064500000616     O          E            findett     2
064600000613     O                                              'Sbagliati e scartati.....:'
064700000613     O                       §CTRNO        2   +  1
064800000613** CMD - COMANDI CL
064900011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T8P)  FORMTYPE(RICCLI) OUTQ(
065000011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T8PS) FORMTYPE(RICCLI) OUTQ(
065100010330DLTOVR FILE(TIS7T8P TIS7T8PS) LVL(*)
065200000613
065300000613
