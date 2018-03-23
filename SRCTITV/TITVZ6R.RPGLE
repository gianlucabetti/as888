000100070718      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021014     FFIVABwwr  O    E             DISK    usropn
000700070718     Ftitvx2p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900070718     Ftitvx2ps  O    f  198        PRINTER usropn
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
002300041025     D tisi95ds      e ds
002400010330     D esito           s              1
002500010330     D prmlit          s             10
002600010330     D prmfir          s             10
002700010330     D wrkesito        s                   like(esito)
002800010330     D wrkdata         s               d
002900010330     D wrkora          s               t
003000010330     D rrnum           s              6  0 INZ(*zeros)
003100010330     D recko           s            150    INZ(*blanks)
003200011113     D depcmd          s            150    INZ(*blanks)
003300010330     D*------------------
003400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003500010330     D*------------------
003600010330     D WLBDA8          DS                  INZ
003700010330     D  G08DAT                 1      8  0
003800010330     D  G08INV                 9     16  0
003900010330     D  G08ERR                17     17
004000010330     D  G08TGI                18     22  0
004100000313     D*
004200010330
004300010330
004400010330
004500990915     C                   time                    wrkdata
004600990915     C                   time                    wrkora
004700000913     C                   reset                   rrnum
004800990921     C                   reset                   esito
004900990921     C                   reset                   wrkesito
005000010601     C*
005100010601     C                   exsr      opeini
005200041025     C                   exsr      rwvab
005300041025     C*
005400041025     C* Esegue lancio TISI95R solo x chiusura
005500041025     C                   CLEAR                   TISI95DS
005600041025     C                   EVAL      I95TLA = 'C'
005700041025     C                   CALL      'TISI95R'
005800041025     C                   PARM                    TISI95DS
005900010601     C*
006000010601     C                   seton                                        lr
006100010601
006200010601
006300010601
006400010601
006500010601     C*--------------------------------------------------------
006600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006700010601     C*--------------------------------------------------------
006800010601     C     PREELA        BEGSR
006900010601     C*
007000010601     C* SE OCCORRE SPEDIRE IN FILIALE
007100010601     C                   if        invfil <> *zeros and
007200010601     C                             flgGiro = '0'
007300010601     C*
007400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007500010601     C                   eval      flgGiro = '1'
007600000724     C*
007700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007900010601     C     invfil        chain     azorg01l
008000000724     C                   if        %found
008100000724     C                   movel(p)  CMD(1)        depcmd
008200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008300000724     C*
008400000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008500011113     C                   Z-ADD     150           LENGH            15 5
008600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008700000724     C                   PARM                    depcmd
008800000724     C                   PARM                    LENGH
008900000724     C*
009000000724     C                   endif
009100000616     C*
009200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009300000616     C                   movel(p)  CMD(2)        depcmd
009400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009500000616     C*
009600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009700011113     C                   Z-ADD     150           LENGH            15 5
009800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009900000616     C                   PARM                    depcmd
010000000616     C                   PARM                    LENGH
010100000613     C*
010200070718     C                   if        not %open(titvx2ps)
010300070718     C                   open      titvx2ps
010400000616     C                   except    testdett
010500000613     C                   endif
010600010601     C*
010700010601     C                   endif
010800010601     C*
010900010601     C                   ENDSR
011000010601     C***
011100010601
011200010601
011300010601
011400010601     C*--------------------------------------------------------
011500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011600010601     C*--------------------------------------------------------
011700010601     C     ENDELA        BEGSR
011800990915     C*
011900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012000000613     C*
012100070718     C                   if        %open(titvx2ps)
012200000616     C                   except    findett
012300070718     C                   close     titvx2ps
012400000613     C                   endif
012500000616     C*
012600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012700011113     C                   Z-ADD     150           LENGH            15 5
012800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012900000616     C                   PARM                    CMD(3)
013000000616     C                   PARM                    LENGH
013100000616     C*
013200010601     C                   ENDSR
013300010601     C***
013400010601
013500010601
013600000613
013700000613     C*--------------------------------------------------------
013800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013900000613     C*--------------------------------------------------------
014000000621     C     STPR          BEGSR
014100000613     C*
014200070718     C                   if        not %open(titvx2p)
014300070718     C                   open      titvx2p
014400990915     C                   endif
014500990915     C*
014600990915     C                   except    riepilogo
014700990915     C*
014800070718     C                   if        %open(titvx2p)
014900070718     C                   close     titvx2p
015000990914     C                   endif
015100000613     C*
015200000613     C                   ENDSR
015300000613     C***
015400000613
015500010601
015600010601
015700010330     C*--------------------------------------------------------
015800040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015900010330     C*--------------------------------------------------------
016000010605     C     RWVAB         BEGSR
016100010330     C*
016200010330     C                   if        not %open(tivin00r)
016300010330     C                   open      tivin00r
016400010330     C                   endif
016500021014     C                   if        not %open(fivabwwr)
016600021014     C                   open      fivabwwr
016700010330     C                   endif
016800010330     C*
016900010604     C                   clear                   §CTROK
017000010604     C                   clear                   §CTRMO
017100010604     C                   clear                   §CTRNO
017200010330     C*
017300010330     C                   DO        *HIVAL
017400010330     C*
017500010330     C                   READ      tivin00r                               70
017600010618     C*
017700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017800010618     C                   if        vinflg = '1'
017900010618     C                   eval      flgOk = '1'
018000010618     C                   endif
018100010618     C*
018200010330     C                   if        vindta > *blanks
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
019700010605     C                   exsr      impvab
019800010601     C*
019900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020000010605     C                   eval      depfil = VABLNP
020100010601     C                   exsr      repfil
020200010601     C                   if        depfil = invfil
020300021025     C                   if        vlrpoi = 999
020400021025     C                   MOVE(P)   invfil        VABFGS
020500021025     C                   else
020600021025     C                   MOVE(P)   vlrpoi        VABFGS
020700021025     C                   endif
020800010601     C*
020900010601     C                   exsr      PREELA
021000010601     C*
021100010604     C* Ebbene...
021200010604     C*
021300010604     C  N31              ADD       1             §CTROK            7 0
021400010604     C   32              ADD       1             §CTRMO            7 0
021500010604     C   31              ADD       1             §CTRNO            7 0
021600020205     C*
021700040301     C* Scarico il buffer del FIVAB
021800021014     C  N31              WRITE     FIVAB000
021900010604     C*
022000020722     C                   endif
022100020722     C*
022200010604     C                   if        *in31 = *off and
022300010604     C                             *in32 = *off
022400010604     C                   eval      vinflg = '1'
022500010604     C                   else
022600010604     C                   eval      recko = vindta
022700010604     C                   except    rigadett
022800010604     C                   eval      vinflg = '2'
022900010604     C                   endif
023000010604     C                   endif
023100010604     C*
023200010330     C                   else
023300010330     C                   eval      vinflg = '1'
023400010330     C                   endif
023500010601     C*
023600010601     C  N70              update    tivin000
023700010330     C*
023800010330     C  N70              ENDdo
023900010601     C
024000010601     C*
024100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024200010601     C                   if        cntNonEl = *zeros or
024300010601     C                             flgMulti = '0'
024400010330     C* Se non ci sono record con errori ...
024500010601     C                   if        §ctrno = 0 and
024600010604     C                             §ctrmo = 0 and
024700010601     C                             flgStato <> '2'
024800010330     C* ... restituisco esito OK.
024900010330     C                   eval      wrkesito = '0'
025000010330     C                   else
025100010330     C                   if        §ctrok > 0
025200010330     C                   eval      wrkesito = '1'
025300010330     C                   else
025400010615     C                   if        flgOk = '0'
025500010615     C                   eval      wrkesito = '2'
025600010615     C                   else
025700010615     C                   eval      wrkesito = '6'
025800010615     C                   endif
025900010330     C                   endif
026000010330     C                   endif
026100010601     C                   else
026200010601     C                   eval      wrkesito = '9'
026300010601     C                   endif
026400010330     C*
026500010330     C                   if        %open(tivin00r)
026600010330     C                   close     tivin00r
026700010330     C                   endif
026800021014     C                   if        %open(fivabwwr)
026900021014     C                   close     fivabwwr
027000010330     C                   endif
027100010601     C*
027200010601     C                   if        vlrpoi <> 999
027300010601     C                   eval      invfil = vlrpoi
027400010601     C                   endif
027500010330     C*
027600010330     C                   if        §ctrok > 0
027700010601     C                             and invfil > *zeros
027800010330     C                   exsr      invio
027900010330     C                   endif
028000010601     C*
028100010618     C                   if        flgGiro = '1'
028200010601     C                   exsr      endela
028300010618     C                   endif
028400010330     C*
028500010330     C                   ENDSR
028600010330     C***
028700010330
028800010601
028900010601
029000010601
029100010601
029200010330     C*----------------------------------------------------*
029300020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029400010330     C*----------------------------------------------------*
029500010330     C     INZVAR        BEGSR
029600010330     C*
029700020204     C                   Z-ADD     *zeros        Num5_0            5 0
029800020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029900020322     C                   MOVEL     '0'           FlgCAS            1
030000010330     C*
030100010330     C                   ENDSR
030200010330     C*----------------------------------------------------*
030300020722     C*  IMPOSTAZIONE CAMPI COSTANTI
030400010330     C*----------------------------------------------------*
030500010330     C     DEFCAM        BEGSR
030600010330     C*
030700020204     C* Inizializzo il buffer del record da scrivere
030800021014     C                   CLEAR                   FIVAB000
030900020204     C* Imposto i valori di default...
031000070718     C                   EVAL      VABCCM = 1110677
031100070718     C                   EVAL      VABLNP = 111
031200040713     C                   EVAL      VABCTR = 000
031300040923     C                   EVAL      VABCBO = '1'
031400020204     C* ... e poi verifico se sono stati passati come parametri
031500020204     C                   IF        vlrppt > *blanks
031600040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031800020204     C                   EXSR      CHKNUM
031900020204     C                   IF        PiInt=*on
032000020204     C                   Z-ADD     PiVal         VABCCM
032100020204     C                   ENDIF
032200040301     C                   ENDIF
032300040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
032400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032500020204     C                   EXSR      CHKNUM
032600020204     C                   IF        PiInt=*on
032700020204     C                   Z-ADD     PiVal         VABLNP
032800020204     C                   ENDIF
032900040301     C                   ENDIF
033000040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
033100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033200020204     C                   EXSR      CHKNUM
033300020204     C                   IF        PiInt=*on
033400020204     C                   Z-ADD     PiVal         VABCTR
033500040301     C                   ENDIF
033600020204     C                   ENDIF
033700020204     C                   ENDIF
033800020204     C*
033900010330     C                   ENDSR
034000010607     C*----------------------------------------------------*
034100021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034200010607     C*----------------------------------------------------*
034300010607     C     IMPVAB        BEGSR
034400010607     C*
034500010607     C                   SETOFF                                       3132
034600010607     C*
034700010607     C* Reperimento campi ALFA
034800010607     C*
034900010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
035000041012     C                   EVAL      VABRSD=%trim(%subst(vindta:156:35))
035100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035300020117     C* ==
035400041012     C                   EVAL      VABRD2=%trim(%subst(vindta:191:35))
035500041012     C                   EVAL      VABIND=%trim(%subst(vindta:226:35))
035600041012     C                   EVAL      VABLOD=%trim(%subst(vindta:261:35))
035700041012     C                   EVAL      VABPRD=%trim(%subst(vindta:323:2))
035800071204     C                   EVAL      VABRMA=%trim(%subst(vindta:80:15))
035900041012     C                   EVAL      VABNOT=%trim(%subst(vindta:464:35))
036000041012     C                   EVAL      VABNT2=%trim(%subst(vindta:464+35:60-35))
036100010607     C*
036200010607     C* Reperimento campi NUMERICI
036300020722     C                   MOVEL     DATCOR        VABAAS
036400020722     C                   MOVE      DATCOR        VABMGS
036500071204     C* NSP
036600041012     C                   EVAL      PiStr=%trim(%subst(vindta:80:15))
036700020304     C                   EXSR      CHKNUM
036800020304     C                   IF        PiInt=*on
036900040301     C                   Z-ADD     PiVal         VABNSP
037000020304     C                   ELSE
037100020722     C                   SETON                                        31
037200020304     C                   Z-ADD     *zeros        VABNSP
037300020304     C                   EVAL      vinmsg = %trimr(vinmsg)
037400071204     C                             + ' ' + 'VABNSP'
037500020304     C                   ENDIF
037600071204     C* RMN
037700071204     C                   EVAL      PiStr=%trim(%subst(vindta:62:6))
037800071204     C                   EXSR      CHKNUM
037900071204     C                   IF        PiInt=*on
038000071204     C                   Z-ADD     PiVal         VABRMN
038100071204     C                   ELSE
038200071204     C                   SETON                                        31
038300071204     C                   Z-ADD     *zeros        VABRMN
038400071204     C                   EVAL      vinmsg = %trimr(vinmsg)
038500071204     C                             + ' ' + 'VABRMN'
038600071204     C                   ENDIF
038700040420     C* CAD
038800041012     C                   EVAL      PiStr=%trim(%subst(vindta:316:7))
038900010607     C                   EXSR      CHKNUM
039000010607     C                   IF        PiInt=*on
039100020204     C                   Z-ADD     PiVal         Num5_0
039200020204     C                   MOVEL(p)  Num5_0        VABCAD
039300010607     C                   ELSE
039400010607     C                   SETON                                        32
039500020204     C                   EVAL      VABCAD = *zeros
039600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039700020204     C                             + ' ' + 'VABCAD'
039800010607     C                   ENDIF
039900041025     C* Reperisco la provincia dal CAP e dalla località
040000041025     C                   IF        VABPRD  = *blanks AND
040100041025     C                             VABCAD <> *blanks
040200041025     C                   CLEAR                   TISI95DS
040300041025     C                   EVAL      I95TCN = '3'
040400041025     C                   Z-ADD     datcor        I95DAT
040500041025     C                   EVAL      I95CAP = VABCAD
040600041025     C                   EVAL      I95LOC = VABLOD
040700041025     C                   CALL      'TISI95R'
040800041025     C                   PARM                    TISI95DS
040900041025     C                   EVAL      VABPRD = O95PRV
041000041025     C                   ENDIF
041100040420     C* NCL
041200041012     C                   EVAL      PiStr=%trim(%subst(vindta:355:5))
041300010607     C                   EXSR      CHKNUM
041400010607     C                   IF        PiInt=*on
041500010607     C                   Z-ADD     PiVal         VABNCL
041600010607     C                   ELSE
041700010607     C                   SETON                                        32
041800010607     C                   Z-ADD     *zeros        VABNCL
041900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042000010607     C                             + ' ' + 'VABNCL'
042100010607     C                   ENDIF
042200040420     C* PKB
042300041012     C                   EVAL      PiStr=%trim(%subst(vindta:360:7))
042400010607     C                   EXSR      CHKNUM
042500010607     C                   IF        PiNum=*on
042600041012     C                   EVAL(H)   VABPKB = PiVal / 100                         * gestisco 2 dec.
042700010607     C                   ELSE
042800010607     C                   SETON                                        32
042900010607     C                   Z-ADD     *zeros        VABPKB
043000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043100010607     C                             + ' ' + 'VABPKB'
043200010607     C                   ENDIF
043300041012     C* VLB
043400041012     C                   EVAL      PiStr=%trim(%subst(vindta:385:7))
043500041012     C                   EXSR      CHKNUM
043600041012     C                   IF        PiNum=*on
043700041012     C                   EVAL(H)   VABVLB = PiVal / 100                         * gestisco 2 dec.
043800041012     C                   ELSE
043900041012     C                   SETON                                        32
044000041012     C                   Z-ADD     *zeros        VABVLB
044100041012     C                   EVAL      vinmsg = %trimr(vinmsg)
044200041012     C                             + ' ' + 'VABVLB'
044300041012     C                   ENDIF
044400041012     C* DCR
044500041012     C                   IF        %trim(%subst(vindta:529:1)) = '1'
044600041012     C                   EVAL      PiStr=%trim(%subst(vindta:54:8))
044700041012     C                   EXSR      CHKNUM
044800041012     C                   IF        PiInt=*on
044900041012     C                   Z-ADD     PiVal         VABDCR
045000041012     C                   ELSE
045100041012     C                   SETON                                        32
045200041012     C                   Z-ADD     *zeros        VABDCR
045300041012     C                   EVAL      vinmsg = %trimr(vinmsg)
045400041012     C                             + ' ' + 'VABDCR'
045500041012     C                   ENDIF
045600041012     C                   ENDIF
045700041012     C* CAS/VCA
045800041012     C                   IF        %trim(%subst(vindta:392:4)) = 'EUR'
045900041012     C                   EVAL      VABVCA = %trim(%subst(vindta:392:4))
046000041012     C                   MOVEL     '1'           FlgCAS
046100041012     C* Determino il tipo incasso contrassegno (trascodifica)
046200070718     C*                  IF        %trim(%subst(vindta:524:1)) = 'A'
046300070718     C*                  EVAL      VABTIC = *blanks
046400070718     C*                  ENDIF
046500041012     C                   IF        %trim(%subst(vindta:524:1)) = 'B'
046600041012     C                   EVAL      VABTIC = 'BM'
046700041012     C                   ENDIF
046800070718     C*                  IF        %trim(%subst(vindta:524:1)) = 'C'
046900070718     C*                  EVAL      VABTIC = 'CM'
047000070718     C*                  ENDIF
047100070718     C*                  IF        %trim(%subst(vindta:524:1)) = 'D'
047200070718     C*                  EVAL      VABTIC = 'BM'
047300070718     C*                  ENDIF
047400070718     C*                  IF        %trim(%subst(vindta:524:1)) = 'E'
047500070718     C*                  EVAL      VABTIC = 'CM'
047600070718     C*                  ENDIF
047700070718     C                   IF        %trim(%subst(vindta:524:1)) = 'P'
047800070718     C                   EVAL      VABTIC = 'TM'
047900070718     C                   ENDIF
048000041012     C*
048100041012     C                   EVAL      PiStr=%trim(%subst(vindta:396:15))
048200041012     C                   EXSR      CHKNUM
048300041012     C                   IF        PiNum=*on
048400041012     C                   EVAL      VABCAS = PiVal / 1000                        * gestisco 3 dec.
048500041012     C                   ELSE
048600041012     C                   SETON                                        32
048700041012     C                   Z-ADD     *zeros        VABCAS
048800041012     C                   EVAL      vinmsg = %trimr(vinmsg)
048900041012     C                             + ' ' + 'VABCAS'
049000041012     C                   ENDIF
049100041012     C                   ENDIF
049200020322     C*
049300020322     C* Considerazioni finali su CBO/CAS
049400020322     C                   IF        FlgCAS = '1'
049500040713     C                   IF        VABCBO = '1'
049600020322     C                   EVAL      VABCBO = '4'
049700040713     C                   ENDIF
049800040713     C                   IF        VABCBO = '2'
049900040713     C                   EVAL      VABCBO = '6'
050000040713     C                   ENDIF
050100020322     C                   ENDIF
050200020204     C*
050300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050400020204     C                   EXSR      CHKIMPDIV
050500020204     C*
050600010607     C                   ENDSR
050700010607     C*----------------------------------------------------*
050800010601
050900020204
051000020204     C*----------------------------------------------------*
051100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
051200020204     C*----------------------------------------------------*
051300020204     C     CHKIMPDIV     BEGSR
051400020204     C*
051500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
051600020204     C                   Z-ADD     *zeros        wrkDec            9 9
051700020204     C*
051800020204     C* Come prima cosa effettuo considerazioni sulla divisa
051900020204     C                   IF        vabIAS > *zeros
052000020204     C                   IF        vabVAS <> 'EUR'
052100020204     C                   EVAL      vabVAS =  'ITL'
052200020204     C                   ENDIF
052300020204     C                   ENDIF
052400020204     C*
052500020204     C                   IF        vabCAS > *zeros
052600020204     C                   IF        vabVCA <> 'EUR'
052700020204     C                   EVAL      vabVCA =  'ITL'
052800020204     C                   ENDIF
052900020204     C                   ENDIF
053000020204     C*
053100020204     C                   IF        vabVMD > *zeros
053200020321     C                   IF        vabVAD <> 'EUR'
053300020204     C                   EVAL      vabVAD =  'ITL'
053400020204     C                   ENDIF
053500020204     C                   ENDIF
053600020204     C*
053700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
053800020204     C                   Z-ADD     vabIAS        wrkDec
053900020204     C                   IF        wrkDec > *zeros
054000020204     C                   IF        vabVAS = 'ITL'
054100020204     C                   EVAL      vabIAS = *zeros
054200020204     C                   ENDIF
054300020204     C                   ENDIF
054400020204     C*
054500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
054600020204     C                   Z-ADD     vabCAS        wrkDec
054700020204     C                   IF        wrkDec > *zeros
054800020204     C                   IF        vabVCA = 'ITL'
054900020204     C                   EVAL      vabCAS = *zeros
055000020204     C                   ENDIF
055100020204     C                   ENDIF
055200020204     C*
055300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
055400020204     C                   Z-ADD     vabVMD        wrkDec
055500020204     C                   IF        wrkDec > *zeros
055600020204     C                   IF        vabVAD = 'ITL'
055700020204     C                   EVAL      vabVMD = *zeros
055800020204     C                   ENDIF
055900020204     C                   ENDIF
056000020204     C*
056100020204     C                   ENDSR
056200020204     C***
056300020204
056400010330
056500010330
056600010330     C*----------------------------------------------------*
056700010330     C*  CONTROLLO NUMERICITA' CAMPI
056800010330     C*----------------------------------------------------*
056900010330     C     CHKNUM        BEGSR
057000010330     C*
057100010606     C                   IF        PiDecChr = *blanks
057200010606     C                   EVAL      PiDecChr = ','
057300010606     C                   ENDIF
057400010606     C*
057500010606     C                   CALL(e)   'ISNUMERIC'
057600010330     C                   PARM                    PiStr            30
057700010606     C                   PARM                    PiDecChr          1
057800010330     C                   PARM      *ZEROS        PiVal            30 9
057900010330     C                   PARM      '0'           PiInt             1
058000010330     C                   PARM      '0'           PiNum             1
058100010330     C                   IF        %error
058200010606     C                   EVAL      PiNum=*off
058300010330     C                   ENDIF
058400010330     C*
058500010330     C                   ENDSR
058600010330     C***
058700010330
058800010601
058900010601
059000010601
059100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
059200010601     C     repfil        BEGSR
059300010601     C*
059400010601     C                   if        invfil = *zeros and
059500010601     C                             depfil > *zeros and
059600010629     C                             (vinflg = *blanks or
059700010629     C                              vinflg = *zeros)
059800010601     C
059900010601     C                   eval      invfil = depfil
060000010601     C                   endif
060100010601     C*
060200010601     C                   if        depfil <> invfil and
060300010601     C                             invfil > *zeros
060400010601     C                   eval      flgMulti = '1'
060500010601     C                   if        vinflg = *blanks
060600010601     C                   add       1             cntNonEl
060700010601     C                   endif
060800010601     C                   endif
060900010601     C*
061000010601     C                   if        vinflg = '2'
061100010601     C                   eval      flgStato = '2'
061200010601     C                   endif
061300010601     C*
061400010601     C                   ENDSR
061500010601     C***
061600010601
061700010601
061800010601
061900010330
062000010330
062100010330
062200990920      /TITLE Invio dei dati al punto operativo.
062300000613     C     invio         BEGSR
062400990920     C*
062500990920     C                   reset                   dscmz
062600010601     C                   move      invfil        cmzdst
062700990920     C                   eval      cmzfld = vlrfou
062800990920     C                   eval      cmzmbd = vlrhdl
062900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
063000000710     C                   if        prmfir = *blanks
063100021014     C                   eval      cmzfla = 'FIVAB00F'
063200021014     C                   eval      cmzmba = 'FIVAB00F'
063300000710     C                   else
063400000710     C                   eval      cmzfla = prmfir
063500000710     C                   eval      cmzmba = prmfir
063600000710     C                   endif
063700990920     C                   eval      cmznrr = *zeros
063800990920     C                   move      §ctrok        cmznrr
063900021018     C                   eval      cmzlba = vlrfl1
064000990920     C                   call(e)   'TIS711C'
064100990920     C                   parm                    dscmz
064200990921     C                   parm      *blanks       esito
064300990920     C                   if        %error
064400990920     C                             or cmzerr = '1'
064500990921     C                             or esito  = '1'
064600000710     C                   eval      wrkesito = '3'
064700990920     C                   endif
064800990920     C*
064900000613     C                   ENDSR
065000990910
065100010601
065200010601
065300010601
065400010601
065500010601      /TITLE Invio dei dati al punto operativo.
065600010601     C     opeini        BEGSR
065700010601     C*
065800010601     C* Inizializzo flag e contatori operativi
065900010601     C                   movel     '0'           flgGiro           1
066000010601     C                   movel     '0'           flgMulti          1
066100010601     C                   movel     '1'           flgStato          1
066200010615     C                   movel     '0'           flgOk             1
066300010601     C                   z-add     *zeros        cntNonEl         10 0
066400010601     C                   z-add     *zeros        depfil            3 0
066500010601     C                   z-add     *zeros        invfil            3 0
066600010601     C*
066700010601     C                   ENDSR
066800010601     C***
066900010601
067000010601
067100010601
067200010330
067300010330
067400000613     C     *inzsr        BEGSR
067500990910     C*
067600990910     C     *entry        plist
067700990920     C                   parm                    tivlrds
067800990921     C                   parm      wrkesito      esito
067900000724     C                   parm                    prmlit
068000000710     C                   parm                    prmfir
068100010330     C*
068200010330     C* CALCOLA LA DATA CORRENTE
068300010330     C                   time                    wn14             14 0
068400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068500010330     C                   z-add     wn8           g08dat
068600010330     C                   z-add     *zeros        g08inv
068700010330     C                   movel     '0'           g08err
068800010330     C                   call      'XSRDA8'
068900010330     C                   parm                    wlbda8
069000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069100000613     C*
069200000613     C                   ENDSR
069300000613     C***
069400990908
069500070718     Otitvx2p   E            riepilogo         2
069600990915     O                                              'Upload via Internet'
069700990915     O                                           +1 'Traduzione TIVIN00R -'
069800021014     O                                           +1 'FIVABWWR'
069900990915     O                                           +1 'Testate bolle'
070000990915     O          E            riepilogo   2
070100990915     O                       wrkdata
070200990915     O                       wrkora              +1
070300990915     O                       procname            +1
070400990915     O          E            riepilogo   2
070500990915     O                                              'Cliente..................:'
070600010605     O                       VABCCM        z     +1
070700990915     O          E            riepilogo   2
070800990920     O                                              'Riferimento Strategi.....:'
070900990920     O                       vlrhdl              +1
071000990915     O          E            riepilogo   2
071100990915     O                                              'Giusti...................:'
071200971022     O                       §CTROK        2   +  1
071300990915     O          E            riepilogo   2
071400990915     O                                              'Sbagliati e corretti.....:'
071500971022     O                       §CTRMO        2   +  1
071600990915     O          E            riepilogo   2
071700990915     O                                              'Sbagliati e scartati.....:'
071800971022     O                       §CTRNO        2   +  1
071900000613
072000070718     Otitvx2ps  E            testdett          2
072100000613     O                                              'Upload via Internet'
072200000613     O                                           +1 'Traduzione TIVIN00R -'
072300021014     O                                           +1 'FIVABWWR'
072400000613     O                                           +1 'Testate bolle'
072500000616     O          E            testdett    3
072600000613     O                                           +2 'N° rec'
072700000613     O                                           +3 'Anteprima contenuto'
072800000616     O          E            rigadett    2
072900000613     O                       rrnum               +2
073000000621     O                       recko               +3
073100000616     O          E            findett     2
073200000613     O                       wrkdata
073300000613     O                       wrkora              +1
073400000613     O                       procname            +1
073500000616     O          E            findett     2
073600000613     O                                              'Cliente..................:'
073700010605     O                       VABCCM        z     +1
073800000616     O          E            findett     2
073900000613     O                                              'Riferimento Strategi.....:'
074000000613     O                       vlrhdl              +1
074100000616     O          E            findett     2
074200000613     O                                              'Giusti...................:'
074300000613     O                       §CTROK        2   +  1
074400000616     O          E            findett     2
074500000613     O                                              'Sbagliati e corretti.....:'
074600000613     O                       §CTRMO        2   +  1
074700000616     O          E            findett     2
074800000613     O                                              'Sbagliati e scartati.....:'
074900000613     O                       §CTRNO        2   +  1
075000000613** CMD - COMANDI CL
075100070718OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVX2P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
075200070718OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVX2PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
075300070718DLTOVR FILE(TITVX2P TITVX2PS) LVL(*)
075400010606
075500000613
