000100051205      /TITLE Upload via Internet: traduzione in FIVAXWWR. (x GOLDWELL Cod. 1320014)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600051205     FFIVAXwwr  O    E             DISK    usropn
000700051205     Ftitvp4p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900051205     Ftitvp4ps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
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
003100011113     D depcmd          s            150    INZ(*blanks)
003200051205     D wNumRecCorr     s              6  0 INZ(*zeros)
003300051205     D wNumSpedCorr    s                   LIKE(vaxnsp) INZ(*zeros)
003400051205     D wVAXPRR         s                   LIKE(vaxprr) INZ(*zeros)
003500010330     D*------------------
003600010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003700010330     D*------------------
003800010330     D WLBDA8          DS                  INZ
003900010330     D  G08DAT                 1      8  0
004000010330     D  G08INV                 9     16  0
004100010330     D  G08ERR                17     17
004200010330     D  G08TGI                18     22  0
004300000313     D*
004400010330
004500010330
004600010330
004700990915     C                   time                    wrkdata
004800990915     C                   time                    wrkora
004900000913     C                   reset                   rrnum
005000990921     C                   reset                   esito
005100990921     C                   reset                   wrkesito
005200010601     C*
005300010601     C                   exsr      opeini
005400051205     C                   exsr      rwvax
005500010601     C*
005600010601     C                   seton                                        lr
005700010601
005800010601
005900010601
006000010601
006100010601     C*--------------------------------------------------------
006200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006300010601     C*--------------------------------------------------------
006400010601     C     PREELA        BEGSR
006500010601     C*
006600010601     C* SE OCCORRE SPEDIRE IN FILIALE
006700010601     C                   if        invfil <> *zeros and
006800010601     C                             flgGiro = '0'
006900010601     C*
007000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007100010601     C                   eval      flgGiro = '1'
007200000724     C*
007300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007500010601     C     invfil        chain     azorg01l
007600000724     C                   if        %found
007700000724     C                   movel(p)  CMD(1)        depcmd
007800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007900000724     C*
008000000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008100011113     C                   Z-ADD     150           LENGH            15 5
008200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008300000724     C                   PARM                    depcmd
008400000724     C                   PARM                    LENGH
008500000724     C*
008600000724     C                   endif
008700000616     C*
008800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008900000616     C                   movel(p)  CMD(2)        depcmd
009000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009100000616     C*
009200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009300011113     C                   Z-ADD     150           LENGH            15 5
009400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009500000616     C                   PARM                    depcmd
009600000616     C                   PARM                    LENGH
009700000613     C*
009800051205     C                   if        not %open(titvp4ps)
009900051205     C                   open      titvp4ps
010000000616     C                   except    testdett
010100000613     C                   endif
010200010601     C*
010300010601     C                   endif
010400010601     C*
010500010601     C                   ENDSR
010600010601     C***
010700010601
010800010601
010900010601
011000010601     C*--------------------------------------------------------
011100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011200010601     C*--------------------------------------------------------
011300010601     C     ENDELA        BEGSR
011400990915     C*
011500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011600000613     C*
011700051205     C                   if        %open(titvp4ps)
011800000616     C                   except    findett
011900051205     C                   close     titvp4ps
012000000613     C                   endif
012100000616     C*
012200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012300011113     C                   Z-ADD     150           LENGH            15 5
012400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012500000616     C                   PARM                    CMD(3)
012600000616     C                   PARM                    LENGH
012700000616     C*
012800010601     C                   ENDSR
012900010601     C***
013000010601
013100010601
013200010601
013300000613
013400000613     C*--------------------------------------------------------
013500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013600000613     C*--------------------------------------------------------
013700000621     C     STPR          BEGSR
013800000613     C*
013900051205     C                   if        not %open(titvp4p)
014000051205     C                   open      titvp4p
014100990915     C                   endif
014200990915     C*
014300990915     C                   except    riepilogo
014400990915     C*
014500051205     C                   if        %open(titvp4p)
014600051205     C                   close     titvp4p
014700990914     C                   endif
014800000613     C*
014900000613     C                   ENDSR
015000000613     C***
015100000613
015200010330
015300010601
015400010601
015500010330     C*--------------------------------------------------------
015600051205     C* RWVAX   LEGGE TIVIN00R E SCRIVE FIVAXWWF              *
015700010330     C*--------------------------------------------------------
015800051205     C     RWVAX         BEGSR
015900010330     C*
016000010330     C                   if        not %open(tivin00r)
016100010330     C                   open      tivin00r
016200010330     C                   endif
016300051205     C                   if        not %open(fivaxwwr)
016400051205     C                   open      fivaxwwr
016500010330     C                   endif
016600010330     C*
016700010604     C                   clear                   §CTROK
016800010604     C                   clear                   §CTRMO
016900010604     C                   clear                   §CTRNO
017000010330     C*
017100010330     C                   DO        *HIVAL
017200010330     C*
017300010330     C                   READ      tivin00r                               70
017400051027     C                   ADD       1             wNumRecCorr
017500010618     C*
017600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017700010618     C                   if        vinflg = '1'
017800010618     C                   eval      flgOk = '1'
017900010618     C                   endif
018000010618     C*
018100051027     C                   if        vindta > *blanks AND
018200051027     C                             wNumRecCorr >= 3
018300010330     C                   add       1             rrnum
018400010330     C*
018500010601     C                   if        *in70 = *off and
018600010330     C                             (vinflg = *blanks
018700010330     C                              or vinflg = '0'
018800010330     C                              or vinflg = '2')
018900010330     C*
019000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019100010711     C                   if        vinflg = *blanks or vinflg = '0'
019200010711     C                   clear                   vinmsg
019300010711     C                   endif
019400010601     C*
019500010330     C                   exsr      inzvar
019600010330     C                   exsr      defcam
019700051205     C                   exsr      impvax
019800051206     C*
019900051206     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020000051206     C                   eval      depfil = VAXLNP
020100051206     C                   exsr      repfil
020200051206     C                   if        depfil = invfil
020300051206     C                   exsr      CALFGS
020400051206     C*
020500010601     C                   exsr      PREELA
020600010601     C*
020700010604     C* Ebbene...
020800010604     C*
020900010604     C  N31              ADD       1             §CTROK            7 0
021000010604     C   32              ADD       1             §CTRMO            7 0
021100010604     C   31              ADD       1             §CTRNO            7 0
021200020205     C*
021300051205     C  N31              WRITE     FIVAX000
021400020722     C*
021500010604     C                   if        *in31 = *off and
021600010604     C                             *in32 = *off
021700010604     C                   eval      vinflg = '1'
021800010604     C                   else
021900010604     C                   eval      recko = vindta
022000010604     C                   except    rigadett
022100010604     C                   eval      vinflg = '2'
022200010604     C                   endif
022300010604     C                   endif
022400010601     C*
022500010604     C                   endif
022600010604     C*
022700010330     C                   else
022800010330     C                   eval      vinflg = '1'
022900010330     C                   endif
023000010601     C*
023100010601     C  N70              update    tivin000
023200010330     C*
023300010330     C  N70              ENDdo
023400010601     C
023500010601     C*
023600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023700010601     C                   if        cntNonEl = *zeros or
023800010601     C                             flgMulti = '0'
023900010330     C* Se non ci sono record con errori ...
024000010601     C                   if        §ctrno = 0 and
024100010604     C                             §ctrmo = 0 and
024200010601     C                             flgStato <> '2'
024300010330     C* ... restituisco esito OK.
024400010330     C                   eval      wrkesito = '0'
024500010330     C                   else
024600010330     C                   if        §ctrok > 0
024700010330     C                   eval      wrkesito = '1'
024800010330     C                   else
024900010615     C                   if        flgOk = '0'
025000010615     C                   eval      wrkesito = '2'
025100010615     C                   else
025200010615     C                   eval      wrkesito = '6'
025300010615     C                   endif
025400010330     C                   endif
025500010330     C                   endif
025600010601     C                   else
025700010601     C                   eval      wrkesito = '9'
025800010601     C                   endif
025900010330     C*
026000010330     C                   if        %open(tivin00r)
026100010330     C                   close     tivin00r
026200010330     C                   endif
026300051205     C                   if        %open(fivaxwwr)
026400051205     C                   close     fivaxwwr
026500010330     C                   endif
026600010601     C*
026700010601     C                   if        vlrpoi <> 999
026800010601     C                   eval      invfil = vlrpoi
026900010601     C                   endif
027000010330     C*
027100010330     C                   if        §ctrok > 0
027200010601     C                             and invfil > *zeros
027300010330     C                   exsr      invio
027400010330     C                   endif
027500010601     C*
027600010618     C                   if        flgGiro = '1'
027700010601     C                   exsr      endela
027800010618     C                   endif
027900010330     C*
028000010330     C                   ENDSR
028100010330     C***
028200010330
028300010601
028400010601
028500010601
028600010601
028700010330     C*----------------------------------------------------*
028800020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028900010330     C*----------------------------------------------------*
029000010330     C     INZVAR        BEGSR
029100010330     C*
029200051205     C*
029300010330     C                   ENDSR
029400010330     C*----------------------------------------------------*
029500020722     C*  IMPOSTAZIONE CAMPI COSTANTI
029600010330     C*----------------------------------------------------*
029700010330     C     DEFCAM        BEGSR
029800010330     C*
029900020204     C* Inizializzo il buffer del record da scrivere
030000051205     C                   CLEAR                   FIVAX000
030100020204     C* Imposto i valori di default...
030200051205     C                   EVAL      VAXCCM = 1320014
030300051205     C                   EVAL      VAXLNP = 132
030400020204     C* ... e poi verifico se sono stati passati come parametri
030500020204     C                   IF        vlrppt > *blanks
030600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030700020204     C                   EXSR      CHKNUM
030800020204     C                   IF        PiInt=*on
030900051205     C                   Z-ADD     PiVal         VAXCCM
031000020204     C                   ENDIF
031100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031200020204     C                   EXSR      CHKNUM
031300020204     C                   IF        PiInt=*on
031400051205     C                   Z-ADD     PiVal         VAXLNP
031500020204     C                   ENDIF
031600020204     C                   ENDIF
031700020204     C*
031800010330     C                   ENDSR
031900010607     C*----------------------------------------------------*
032000051205     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAX)
032100010607     C*----------------------------------------------------*
032200051205     C     IMPVAX        BEGSR
032300010607     C*
032400010607     C                   SETOFF                                       3132
032500010607     C*
032600010607     C* Reperimento campi ALFA
032700051205     C                   EVAL      VAXTMD=%trim(%subst(vindta:81:4))
032800051205     C                   EVAL      VAXTRK=%trim(%subst(vindta:102:2))
032900010607     C*
033000010607     C* Reperimento campi NUMERICI
033100051205     C                   MOVEL     datcor        VAXAAS
033200051205     C* CCM
033300051205     C                   EVAL      PiStr=%trim(%subst(vindta:13:7))
033400051205     C                   EXSR      CHKNUM
033500051205     C                   IF        PiInt=*on
033600051205     C                   Z-ADD     PiVal         VAXCCM
033700051205     C                   ELSE
033800051205     C                   SETON                                        32
033900051205     C                   EVAL      vinmsg = %trimr(vinmsg)
034000051205     C                             + ' ' + 'VAXCCM'
034100051205     C                   ENDIF
034200051205     C* LNP
034300051205     C                   EVAL      PiStr=%trim(%subst(vindta:30:3))
034400051205     C                   EXSR      CHKNUM
034500051205     C                   IF        PiInt=*on
034600051205     C                   Z-ADD     PiVal         VAXLNP
034700051205     C                   ELSE
034800051205     C                   SETON                                        32
034900051205     C                   EVAL      vinmsg = %trimr(vinmsg)
035000051205     C                             + ' ' + 'VAXLNP'
035100051205     C                   ENDIF
035200051205     C* NRS
035300051205     C                   EVAL      PiStr=%trim(%subst(vindta:48:2))
035400051205     C                   EXSR      CHKNUM
035500051205     C                   IF        PiInt=*on
035600051205     C                   Z-ADD     PiVal         VAXNRS
035700051205     C                   ELSE
035800051205     C                   SETON                                        32
035900051205     C                   Z-ADD     *zeros        VAXNRS
036000051205     C                   EVAL      vinmsg = %trimr(vinmsg)
036100051205     C                             + ' ' + 'VAXNRS'
036200051205     C                   ENDIF
036300051205     C* NSP
036400051205     C                   EVAL      PiStr=%trim(%subst(vindta:74:5))
036500030130     C                   EXSR      CHKNUM
036600030130     C                   IF        PiInt=*on
036700051205     C                   Z-ADD     PiVal         VAXNSP
036800030130     C                   ELSE
036900030130     C                   SETON                                        31
037000051205     C                   Z-ADD     *zeros        VAXNSP
037100030130     C                   EVAL      vinmsg = %trimr(vinmsg)
037200051205     C                             + ' ' + 'VAXNSP'
037300030130     C                   ENDIF
037400051205     C*
037500051205     C* Effettuo test particolare sul numero spedizione x poi gestire correttamente
037600051205     C* il progressivo riga => attribuito a programma
037700051205     C                   IF        VAXNSP <> wNumSpedCorr
037800051205     C                   EVAL      wNumSpedCorr = VAXNSP
037900051206     C*
038000051206     C* Gestisco da traduttore l'inserimento d 8 righe vuote x ospitare degnamente il pessimo
038100051206     C* logo che il cliente c ha fornito
038200051206     C                   FOR       wVAXPRR = 1 to 8
038300051206     C                   EXSR      CALFGS
038400051206     C                   EVAL      VAXPRR = wVAXPRR
038500051206     C                   WRITE     FIVAX000
038600051206     C                   ENDFOR
038700051206     C                   ELSE
038800051206     C                   ADD       1             wVAXPRR
038900051206     C                   ENDIF
039000051205     C                   EVAL      VAXPRR = wVAXPRR
039100051206     C*
039200051206     C* Campo dati in stampa
039300051206     C                   EVAL      VAXDTT=%trim(%subst(vindta:110:500))
039400051205     C*
039500051205     C* PRR
039600051205     C*                  EVAL      PiStr=%trim(%subst(vindta:96:4))
039700051205     C*                  EXSR      CHKNUM
039800051205     C*                  IF        PiInt=*on
039900051205     C*                  Z-ADD     PiVal         VAXPRR
040000051205     C*                  ELSE
040100051205     C*                  SETON                                        32
040200051205     C*                  Z-ADD     *zeros        VAXPRR
040300051205     C*                  EVAL      vinmsg = %trimr(vinmsg)
040400051205     C*                            + ' ' + 'VAXPRR'
040500051205     C*                  ENDIF
040600020204     C*
040700010607     C                   ENDSR
040800010607     C*----------------------------------------------------*
040900051206
041000051206
041100051206
041200051206     C*----------------------------------------------------*
041300051206     C*  DETERMINAZIONE FILIALE GESTIONE
041400051206     C*----------------------------------------------------*
041500051206     C     CALFGS        BEGSR
041600051206     C*
041700051206     C                   if        vlrpoi = 999
041800051206     C                   MOVE(P)   invfil        VAXFGS
041900051206     C                   else
042000051206     C                   MOVE(P)   vlrpoi        VAXFGS
042100051206     C                   endif
042200051206     C*
042300051206     C                   ENDSR
042400051206     C***
042500020204
042600010330
042700010330
042800010330     C*----------------------------------------------------*
042900010330     C*  CONTROLLO NUMERICITA' CAMPI
043000010330     C*----------------------------------------------------*
043100010330     C     CHKNUM        BEGSR
043200010330     C*
043300010606     C                   IF        PiDecChr = *blanks
043400030307     C                   EVAL      PiDecChr = ','
043500010606     C                   ENDIF
043600010606     C*
043700010606     C                   CALL(e)   'ISNUMERIC'
043800010330     C                   PARM                    PiStr            30
043900010606     C                   PARM                    PiDecChr          1
044000010330     C                   PARM      *ZEROS        PiVal            30 9
044100010330     C                   PARM      '0'           PiInt             1
044200010330     C                   PARM      '0'           PiNum             1
044300010330     C                   IF        %error
044400010606     C                   EVAL      PiNum=*off
044500010330     C                   ENDIF
044600010330     C*
044700010330     C                   ENDSR
044800010330     C***
044900010330
045000010601
045100010601
045200010601
045300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
045400010601     C     repfil        BEGSR
045500010601     C*
045600010601     C                   if        invfil = *zeros and
045700010601     C                             depfil > *zeros and
045800010629     C                             (vinflg = *blanks or
045900010629     C                              vinflg = *zeros)
046000010601     C
046100010601     C                   eval      invfil = depfil
046200010601     C                   endif
046300010601     C*
046400010601     C                   if        depfil <> invfil and
046500010601     C                             invfil > *zeros
046600010601     C                   eval      flgMulti = '1'
046700010601     C                   if        vinflg = *blanks
046800010601     C                   add       1             cntNonEl
046900010601     C                   endif
047000010601     C                   endif
047100010601     C*
047200010601     C                   if        vinflg = '2'
047300010601     C                   eval      flgStato = '2'
047400010601     C                   endif
047500010601     C*
047600010601     C                   ENDSR
047700010601     C***
047800010601
047900010601
048000010601
048100010330
048200010330
048300010330
048400990920      /TITLE Invio dei dati al punto operativo.
048500000613     C     invio         BEGSR
048600990920     C*
048700990920     C                   reset                   dscmz
048800010601     C                   move      invfil        cmzdst
048900990920     C                   eval      cmzfld = vlrfou
049000990920     C                   eval      cmzmbd = vlrhdl
049100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
049200000710     C                   if        prmfir = *blanks
049300051205     C                   eval      cmzfla = 'FIVAX00F'
049400051205     C                   eval      cmzmba = 'FIVAX00F'
049500000710     C                   else
049600000710     C                   eval      cmzfla = prmfir
049700000710     C                   eval      cmzmba = prmfir
049800000710     C                   endif
049900990920     C                   eval      cmznrr = *zeros
050000990920     C                   move      §ctrok        cmznrr
050100021018     C                   eval      cmzlba = vlrfl1
050200990920     C                   call(e)   'TIS711C'
050300990920     C                   parm                    dscmz
050400990921     C                   parm      *blanks       esito
050500990920     C                   if        %error
050600990920     C                             or cmzerr = '1'
050700990921     C                             or esito  = '1'
050800000710     C                   eval      wrkesito = '3'
050900990920     C                   endif
051000990920     C*
051100000613     C                   ENDSR
051200990910
051300010601
051400010601
051500010601
051600010601
051700010601      /TITLE Invio dei dati al punto operativo.
051800010601     C     opeini        BEGSR
051900010601     C*
052000010601     C* Inizializzo flag e contatori operativi
052100010601     C                   movel     '0'           flgGiro           1
052200010601     C                   movel     '0'           flgMulti          1
052300010601     C                   movel     '1'           flgStato          1
052400010615     C                   movel     '0'           flgOk             1
052500010601     C                   z-add     *zeros        cntNonEl         10 0
052600010601     C                   z-add     *zeros        depfil            3 0
052700010601     C                   z-add     *zeros        invfil            3 0
052800010601     C*
052900010601     C                   ENDSR
053000010601     C***
053100010601
053200010601
053300010601
053400010330
053500010330
053600000613     C     *inzsr        BEGSR
053700990910     C*
053800990910     C     *entry        plist
053900990920     C                   parm                    tivlrds
054000990921     C                   parm      wrkesito      esito
054100000724     C                   parm                    prmlit
054200000710     C                   parm                    prmfir
054300010330     C*
054400010330     C* CALCOLA LA DATA CORRENTE
054500010330     C                   time                    wn14             14 0
054600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
054700010330     C                   z-add     wn8           g08dat
054800010330     C                   z-add     *zeros        g08inv
054900010330     C                   movel     '0'           g08err
055000010330     C                   call      'XSRDA8'
055100010330     C                   parm                    wlbda8
055200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
055300000613     C*
055400000613     C                   ENDSR
055500000613     C***
055600990908
055700051205     Otitvp4p   E            riepilogo         2
055800990915     O                                              'Upload via Internet'
055900990915     O                                           +1 'Traduzione TIVIN00R -'
056000051205     O                                           +1 'FIVAXWWR'
056100990915     O                                           +1 'Testate bolle'
056200990915     O          E            riepilogo   2
056300990915     O                       wrkdata
056400990915     O                       wrkora              +1
056500990915     O                       procname            +1
056600990915     O          E            riepilogo   2
056700990915     O                                              'Cliente..................:'
056800051205     O                       VAXCCM        z     +1
056900990915     O          E            riepilogo   2
057000990920     O                                              'Riferimento Strategi.....:'
057100990920     O                       vlrhdl              +1
057200990915     O          E            riepilogo   2
057300990915     O                                              'Giusti...................:'
057400971022     O                       §CTROK        2   +  1
057500990915     O          E            riepilogo   2
057600990915     O                                              'Sbagliati e corretti.....:'
057700971022     O                       §CTRMO        2   +  1
057800990915     O          E            riepilogo   2
057900990915     O                                              'Sbagliati e scartati.....:'
058000971022     O                       §CTRNO        2   +  1
058100000613
058200051205     Otitvp4ps  E            testdett          2
058300000613     O                                              'Upload via Internet'
058400000613     O                                           +1 'Traduzione TIVIN00R -'
058500051205     O                                           +1 'FIVAXWWR'
058600000613     O                                           +1 'Testate bolle'
058700000616     O          E            testdett    3
058800000613     O                                           +2 'N° rec'
058900000613     O                                           +3 'Anteprima contenuto'
059000000616     O          E            rigadett    2
059100000613     O                       rrnum               +2
059200000621     O                       recko               +3
059300000616     O          E            findett     2
059400000613     O                       wrkdata
059500000613     O                       wrkora              +1
059600000613     O                       procname            +1
059700000616     O          E            findett     2
059800000613     O                                              'Cliente..................:'
059900051205     O                       VAXCCM        z     +1
060000000616     O          E            findett     2
060100000613     O                                              'Riferimento Strategi.....:'
060200000613     O                       vlrhdl              +1
060300000616     O          E            findett     2
060400000613     O                                              'Giusti...................:'
060500000613     O                       §CTROK        2   +  1
060600000616     O          E            findett     2
060700000613     O                                              'Sbagliati e corretti.....:'
060800000613     O                       §CTRMO        2   +  1
060900000616     O          E            findett     2
061000000613     O                                              'Sbagliati e scartati.....:'
061100000613     O                       §CTRNO        2   +  1
061200000613** CMD - COMANDI CL
061300051205OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVP4P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
061400051205OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVP4PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
061500051205DLTOVR FILE(TITVP4P TITVP4PS) LVL(*)
061600010606
061700000613
