000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700071022     Ftitv1b5p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900071022     Ftitv1b5ps O    f  198        PRINTER usropn
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
002300070502     D tisi95ds      e ds
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
004100010330
004200010330
004300990915     C                   time                    wrkdata
004400990915     C                   time                    wrkora
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800010601     C*
004900010601     C                   exsr      opeini
005000010605     C                   exsr      rwvab
005100070502     C*
005200070502     C* Effettuo lancio TISI95 solo x chiusura
005300070502     C                   CLEAR                   TISI95DS
005400070502     C                   EVAL      I95TLA = 'C'
005500070502     C                   CALL      'TISI95R'
005600070502     C                   PARM                    TISI95DS
005700010601     C*
005800010601     C                   seton                                        lr
005900010601
006000010601
006100010601
006200010601
006300010601     C*--------------------------------------------------------
006400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006500010601     C*--------------------------------------------------------
006600010601     C     PREELA        BEGSR
006700010601     C*
006800010601     C* SE OCCORRE SPEDIRE IN FILIALE
006900010601     C                   if        invfil <> *zeros and
007000010601     C                             flgGiro = '0'
007100010601     C*
007200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007300010601     C                   eval      flgGiro = '1'
007400000724     C*
007500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007700010601     C     invfil        chain     azorg01l
007800000724     C                   if        %found
007900000724     C                   movel(p)  CMD(1)        depcmd
008000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008100000724     C*
008200000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008300011113     C                   Z-ADD     150           LENGH            15 5
008400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008500000724     C                   PARM                    depcmd
008600000724     C                   PARM                    LENGH
008700000724     C*
008800000724     C                   endif
008900000616     C*
009000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009100000616     C                   movel(p)  CMD(2)        depcmd
009200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009300000616     C*
009400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009500011113     C                   Z-ADD     150           LENGH            15 5
009600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009700000616     C                   PARM                    depcmd
009800000616     C                   PARM                    LENGH
009900000613     C*
010000071022     C                   if        not %open(titv1b5ps)
010100071022     C                   open      titv1b5ps
010200000616     C                   except    testdett
010300000613     C                   endif
010400010601     C*
010500010601     C                   endif
010600010601     C*
010700010601     C                   ENDSR
010800010601     C***
010900010601
011000010601
011100010601
011200010601     C*--------------------------------------------------------
011300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011400010601     C*--------------------------------------------------------
011500010601     C     ENDELA        BEGSR
011600990915     C*
011700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011800000613     C*
011900071022     C                   if        %open(titv1b5ps)
012000000616     C                   except    findett
012100071022     C                   close     titv1b5ps
012200000613     C                   endif
012300000616     C*
012400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012500011113     C                   Z-ADD     150           LENGH            15 5
012600010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012700000616     C                   PARM                    CMD(3)
012800000616     C                   PARM                    LENGH
012900000616     C*
013000010601     C                   ENDSR
013100010601     C***
013200010601
013300010601
013400000613
013500000613     C*--------------------------------------------------------
013600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013700000613     C*--------------------------------------------------------
013800000621     C     STPR          BEGSR
013900000613     C*
014000071022     C                   if        not %open(titv1b5p)
014100071022     C                   open      titv1b5p
014200990915     C                   endif
014300990915     C*
014400990915     C                   except    riepilogo
014500990915     C*
014600071022     C                   if        %open(titv1b5p)
014700071022     C                   close     titv1b5p
014800990914     C                   endif
014900000613     C*
015000000613     C                   ENDSR
015100000613     C***
015200000613
015300010601
015400010601
015500010330     C*--------------------------------------------------------
015600050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
015700010330     C*--------------------------------------------------------
015800010605     C     RWVAB         BEGSR
015900050408     C*
016000010330     C                   if        not %open(tivin00r)
016100010330     C                   open      tivin00r
016200010330     C                   endif
016300050408     C                   if        not %open(fivabwwr)
016400050408     C                   open      fivabwwr
016500010330     C                   endif
016600010330     C*
016700010604     C                   clear                   §CTROK
016800010604     C                   clear                   §CTRMO
016900010604     C                   clear                   §CTRNO
017000010330     C*
017100010330     C                   DO        *HIVAL
017200010330     C*
017300010330     C                   READ      tivin00r                               70
017400010618     C*
017500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017600010618     C                   if        vinflg = '1'
017700010618     C                   eval      flgOk = '1'
017800010618     C                   endif
017900010618     C*
018000010330     C                   if        vindta > *blanks
018100010330     C                   add       1             rrnum
018200010330     C*
018300010601     C                   if        *in70 = *off and
018400010330     C                             (vinflg = *blanks
018500010330     C                              or vinflg = '0'
018600010330     C                              or vinflg = '2')
018700010330     C*
018800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018900010711     C                   if        vinflg = *blanks or vinflg = '0'
019000010711     C                   clear                   vinmsg
019100010711     C                   endif
019200070601     C*
019300070601     C                   exsr      inzvar
019400070601     C                   exsr      defcam
019500070601     C                   exsr      impvab
019600010601     C*
019700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019800010605     C                   eval      depfil = VABLNP
019900010601     C                   exsr      repfil
020000010601     C                   if        depfil = invfil
020100021025     C                   if        vlrpoi = 999
020200021025     C                   MOVE(P)   invfil        VABFGS
020300021025     C                   else
020400021025     C                   MOVE(P)   vlrpoi        VABFGS
020500021025     C                   endif
020600010601     C*
020700010601     C                   exsr      PREELA
020800010601     C*
020900010604     C* Ebbene...
021000010604     C*
021100010604     C  N31              ADD       1             §CTROK            7 0
021200010604     C   32              ADD       1             §CTRMO            7 0
021300010604     C   31              ADD       1             §CTRNO            7 0
021400071003     C                   exsr      wrivab                                       => scrivo VAB
021500010604     C*
021600020722     C                   endif
021700020722     C*
021800010604     C                   if        *in31 = *off and
021900010604     C                             *in32 = *off
022000010604     C                   eval      vinflg = '1'
022100010604     C                   else
022200010604     C                   eval      recko = vindta
022300010604     C                   except    rigadett
022400010604     C                   eval      vinflg = '2'
022500010604     C                   endif
022600010604     C                   endif
022700010604     C*
022800010330     C                   else
022900010330     C                   eval      vinflg = '1'
023000010330     C                   endif
023100010601     C*
023200010601     C  N70              update    tivin000
023300010330     C*
023400010330     C  N70              ENDdo
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
026300050408     C                   if        %open(fivabwwr)
026400050408     C                   close     fivabwwr
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
028200070601
028300070601     C*----------------------------------------------------*
028400070601     C*  SCARICAMENTO BUFFER RECORDS VAB
028500070601     C*----------------------------------------------------*
028600070601     C     WRIVAB        BEGSR
028700070601     C*
028800070601     C  N31              WRITE     FIVAB000
028900070601     C*
029000070601     C                   ENDSR
029100010601
029200010601
029300010601
029400010330     C*----------------------------------------------------*
029500020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029600010330     C*----------------------------------------------------*
029700010330     C     INZVAR        BEGSR
029800010330     C*
029900020204     C                   Z-ADD     *zeros        Num5_0            5 0
030000020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030100020322     C                   MOVEL     '0'           FlgCAS            1
030200010330     C*
030300010330     C                   ENDSR
030400010330     C*----------------------------------------------------*
030500020722     C*  IMPOSTAZIONE CAMPI COSTANTI
030600010330     C*----------------------------------------------------*
030700010330     C     DEFCAM        BEGSR
030800010330     C*
030900020204     C* Inizializzo il buffer del record da scrivere
031000050408     C                   CLEAR                   FIVAB000
031100020204     C* Imposto i valori di default...
031200071022     C                   EVAL      VABCCM = 0910922
031300071022     C                   EVAL      VABLNP = 091
031400071022     C                   EVAL      VABCTR = 301
031500071022     C                   EVAL      VABCTM = '7Q'
031600070208     C                   EVAL      VABCBO = '1'
031700020204     C* ... e poi verifico se sono stati passati come parametri
031800020204     C                   IF        vlrppt > *blanks
031900040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
032000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032100020204     C                   EXSR      CHKNUM
032200020204     C                   IF        PiInt=*on
032300020204     C                   Z-ADD     PiVal         VABCCM
032400020204     C                   ENDIF
032500040301     C                   ENDIF
032600040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
032700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032800020204     C                   EXSR      CHKNUM
032900020204     C                   IF        PiInt=*on
033000020204     C                   Z-ADD     PiVal         VABLNP
033100020204     C                   ENDIF
033200040301     C                   ENDIF
033300040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
033400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033500020204     C                   EXSR      CHKNUM
033600020204     C                   IF        PiInt=*on
033700020204     C                   Z-ADD     PiVal         VABCTR
033800040301     C                   ENDIF
033900020204     C                   ENDIF
034000020204     C                   ENDIF
034100020204     C*
034200010330     C                   ENDSR
034300010607     C*----------------------------------------------------*
034400050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034500010607     C*----------------------------------------------------*
034600010607     C     IMPVAB        BEGSR
034700010607     C*
034800010607     C                   SETOFF                                       3132
034900010607     C*
035000010607     C* Reperimento campi ALFA
035100010607     C*
035200010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
035300071022     C                   EVAL      VABRSD=%trim(%subst(vindta:11:40))
035400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035600020117     C* ==
035700071022     C                   EVAL      VABRD2=%trim(%subst(vindta:51:40))
035800071022     C                   EVAL      VABIND=%trim(%subst(vindta:91:32))
035900071022     C                   EVAL      VABLOD=%trim(%subst(vindta:123:32))
036000071022     C                   EVAL      VABPRD=%trim(%subst(vindta:155:2))
036100071022     C                   EVAL      VABNAS=%trim(%subst(vindta:207:15))
036200071022     C                   EVAL      VABRMA=%trim(%subst(vindta:163:16))
036300071022     C                   MOVEL     *blanks       wNOTE            70
036400071022     C                   EVAL      wNOTE=%trim(%subst(vindta:259:27))+
036500071022     C                                   %trim(%subst(vindta:286:27))+
036600071022     C                                   %trim(%subst(vindta:426:50))
036700071022     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
036800071022     C                   EVAL      VABNT2=%trim(%subst(wNOTE:1+35:35))
036900071022     C                   IF        %subst(vindta:162:1)='F' or
037000071022     C                             %subst(vindta:162:1)='C'
037100071022     C                   EVAL      VABCBO='1'
037200071022     C                   ENDIF
037300071022     C                   IF        %subst(vindta:162:1)='V' or
037400071022     C                             %subst(vindta:162:1)='D' or
037500071022     C                             %subst(vindta:162:1)='A'
037600071022     C                   EVAL      VABCBO='2'
037700071022     C                   ENDIF
037800060510     C*
037900060510     C* Reperimento campi NUMERICI
038000060510     C                   MOVEL     DATCOR        VABAAS
038100060510     C                   MOVE      DATCOR        VABMGS
038200060710     C* NSP/RMN
038300071022     C                   EVAL      PiStr=%trim(%subst(vindta:163:16))
038400060510     C                   EXSR      CHKNUM
038500060510     C                   IF        PiInt=*on
038600060510     C                   Z-ADD     PiVal         VABNSP
038700060510     C                   Z-ADD     PiVal         VABRMN
038800060404     C                   ELSE
038900070502     C                   SETON                                        31
039000060510     C                   Z-ADD     *zeros        VABNSP
039100060510     C                   Z-ADD     1             VABRMN
039200060404     C                   EVAL      vinmsg = %trimr(vinmsg)
039300060510     C                             + ' ' + 'VABNSP VABRMN'
039400060404     C                   ENDIF
039500040420     C* CAD
039600071022     C                   EVAL      PiStr=%trim(%subst(vindta:157:5))
039700010607     C                   EXSR      CHKNUM
039800010607     C                   IF        PiInt=*on
039900020204     C                   Z-ADD     PiVal         Num5_0
040000020204     C                   MOVEL(p)  Num5_0        VABCAD
040100010607     C                   ELSE
040200010607     C                   SETON                                        32
040300020204     C                   EVAL      VABCAD = *zeros
040400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040500020204     C                             + ' ' + 'VABCAD'
040600060316     C                   ENDIF
040700070502     C* Reperisco la provincia dal CAP e dalla località
040800070502     C                   IF        VABCAD <> *blanks AND
040900070502     C                             VABPRD  = *blanks
041000070502     C                   CLEAR                   TISI95DS
041100070502     C                   EVAL      I95TCN = '3'
041200070502     C                   Z-ADD     datcor        I95DAT
041300070502     C                   EVAL      I95CAP = VABCAD
041400070502     C                   EVAL      I95LOC = VABLOD
041500070502     C                   EVAL      I95NAR = VABNZD
041600070502     C                   CALL      'TISI95R'
041700070502     C                   PARM                    TISI95DS
041800070502     C                   EVAL      VABPRD = O95PRV
041900070502     C                   ENDIF
042000040420     C* NCL
042100071022     C                   EVAL      PiStr=%trim(%subst(vindta:222:5))
042200010607     C                   EXSR      CHKNUM
042300010607     C                   IF        PiInt=*on
042400070606     C                   Z-ADD     PiVal         VABNCL
042500010607     C                   ELSE
042600010607     C                   SETON                                        32
042700010607     C                   Z-ADD     *zeros        VABNCL
042800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042900010607     C                             + ' ' + 'VABNCL'
043000010607     C                   ENDIF
043100071022     C* DCR
043200071022     C                   IF        %trim(%subst(vindta:201:6)) <> *blanks AND
043300071022     C                             %trim(%subst(vindta:201:6)) <> *zeros  AND
043400071022     C                             %trim(%subst(vindta:201:6)) <> '0'
043500071022     C                   EVAL      PiStr='20'+%trim(%subst(vindta:201:6))
043600071022     C                   EXSR      CHKNUM
043700071022     C                   IF        PiInt=*on
043800071022     C                   Z-ADD     PiVal         VABDCR
043900071022     C                   ELSE
044000071022     C                   SETON                                        32
044100071022     C                   Z-ADD     *zeros        VABDCR
044200071022     C                   EVAL      vinmsg = %trimr(vinmsg)
044300071022     C                             + ' ' + 'VABDCR'
044400071022     C                   ENDIF
044500071022     C                   ENDIF
044600040420     C* PKB
044700071022     C                   EVAL      PiStr=%trim(%subst(vindta:227:5))
044800010607     C                   EXSR      CHKNUM
044900010607     C                   IF        PiNum=*on
045000071003     C                   Z-ADD(H)  PiVal         VABPKB
045100010607     C                   ELSE
045200010607     C                   SETON                                        32
045300010607     C                   Z-ADD     *zeros        VABPKB
045400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
045500010607     C                             + ' ' + 'VABPKB'
045600010607     C                   ENDIF
045700071022     C* CAS
045800071022     C                   IF        %subst(vindta:313:13) <> *blanks AND
045900071022     C                             %subst(vindta:313:13) <> *zeros  AND
046000071022     C                             %subst(vindta:313:13) <> '0'
046100071022     C                   EVAL      FlgCAS = '1'
046200071022     C                   EVAL      PiStr=%trim(%subst(vindta:313:13))
046300071022     C                   EXSR      CHKNUM
046400071022     C                   IF        PiNum=*on
046500071022     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
046600071022     C                   Z-ADD(H)  PiVal         VABCAS
046700071022     C                   EVAL      VABVCA ='EUR'
046800071022     C                   ELSE
046900071022     C                   SETON                                        32
047000071022     C                   Z-ADD     *zeros        VABCAS
047100071022     C                   EVAL      vinmsg = %trimr(vinmsg)
047200071022     C                             + ' ' + 'VABCAS'
047300071022     C                   ENDIF
047400071022     C                   ENDIF
047500020322     C*
047600020322     C* Considerazioni finali su CBO/CAS
047700050908     C                   IF        FlgCAS = '1'
047800040713     C                   IF        VABCBO = '1'
047900020322     C                   EVAL      VABCBO = '4'
048000040713     C                   ENDIF
048100040713     C                   IF        VABCBO = '2'
048200040713     C                   EVAL      VABCBO = '6'
048300040713     C                   ENDIF
048400020322     C                   ENDIF
048500020204     C*
048600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048700020204     C                   EXSR      CHKIMPDIV
048800020204     C*
048900010607     C                   ENDSR
049000010607     C*----------------------------------------------------*
049100010601
049200020204
049300020204     C*----------------------------------------------------*
049400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049500020204     C*----------------------------------------------------*
049600020204     C     CHKIMPDIV     BEGSR
049700020204     C*
049800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049900020204     C                   Z-ADD     *zeros        wrkDec            9 9
050000020204     C*
050100020204     C* Come prima cosa effettuo considerazioni sulla divisa
050200020204     C                   IF        vabIAS > *zeros
050300020204     C                   IF        vabVAS <> 'EUR'
050400020204     C                   EVAL      vabVAS =  'ITL'
050500020204     C                   ENDIF
050600020204     C                   ENDIF
050700020204     C*
050800020204     C                   IF        vabCAS > *zeros
050900020204     C                   IF        vabVCA <> 'EUR'
051000020204     C                   EVAL      vabVCA =  'ITL'
051100020204     C                   ENDIF
051200020204     C                   ENDIF
051300020204     C*
051400020204     C                   IF        vabVMD > *zeros
051500020321     C                   IF        vabVAD <> 'EUR'
051600020204     C                   EVAL      vabVAD =  'ITL'
051700020204     C                   ENDIF
051800020204     C                   ENDIF
051900020204     C*
052000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
052100020204     C                   Z-ADD     vabIAS        wrkDec
052200020204     C                   IF        wrkDec > *zeros
052300020204     C                   IF        vabVAS = 'ITL'
052400020204     C                   EVAL      vabIAS = *zeros
052500020204     C                   ENDIF
052600020204     C                   ENDIF
052700020204     C*
052800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
052900020204     C                   Z-ADD     vabCAS        wrkDec
053000020204     C                   IF        wrkDec > *zeros
053100020204     C                   IF        vabVCA = 'ITL'
053200020204     C                   EVAL      vabCAS = *zeros
053300020204     C                   ENDIF
053400020204     C                   ENDIF
053500020204     C*
053600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053700020204     C                   Z-ADD     vabVMD        wrkDec
053800020204     C                   IF        wrkDec > *zeros
053900020204     C                   IF        vabVAD = 'ITL'
054000020204     C                   EVAL      vabVMD = *zeros
054100020204     C                   ENDIF
054200020204     C                   ENDIF
054300020204     C*
054400020204     C                   ENDSR
054500020204     C***
054600020204
054700010330
054800010330
054900010330     C*----------------------------------------------------*
055000010330     C*  CONTROLLO NUMERICITA' CAMPI
055100010330     C*----------------------------------------------------*
055200010330     C     CHKNUM        BEGSR
055300010330     C*
055400010606     C                   IF        PiDecChr = *blanks
055500070502     C                   EVAL      PiDecChr = '.'
055600010606     C                   ENDIF
055700010606     C*
055800010606     C                   CALL(e)   'ISNUMERIC'
055900010330     C                   PARM                    PiStr            30
056000010606     C                   PARM                    PiDecChr          1
056100010330     C                   PARM      *ZEROS        PiVal            30 9
056200010330     C                   PARM      '0'           PiInt             1
056300010330     C                   PARM      '0'           PiNum             1
056400010330     C                   IF        %error
056500010606     C                   EVAL      PiNum=*off
056600010330     C                   ENDIF
056700010330     C*
056800010330     C                   ENDSR
056900010330     C***
057000010330
057100010601
057200010601
057300010601
057400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
057500010601     C     repfil        BEGSR
057600010601     C*
057700010601     C                   if        invfil = *zeros and
057800010601     C                             depfil > *zeros and
057900010629     C                             (vinflg = *blanks or
058000010629     C                              vinflg = *zeros)
058100010601     C
058200010601     C                   eval      invfil = depfil
058300010601     C                   endif
058400010601     C*
058500010601     C                   if        depfil <> invfil and
058600010601     C                             invfil > *zeros
058700010601     C                   eval      flgMulti = '1'
058800010601     C                   if        vinflg = *blanks
058900010601     C                   add       1             cntNonEl
059000010601     C                   endif
059100010601     C                   endif
059200010601     C*
059300010601     C                   if        vinflg = '2'
059400010601     C                   eval      flgStato = '2'
059500010601     C                   endif
059600010601     C*
059700010601     C                   ENDSR
059800010601     C***
059900010601
060000010601
060100010601
060200010330
060300010330
060400010330
060500990920      /TITLE Invio dei dati al punto operativo.
060600000613     C     invio         BEGSR
060700990920     C*
060800990920     C                   reset                   dscmz
060900010601     C                   move      invfil        cmzdst
061000990920     C                   eval      cmzfld = vlrfou
061100990920     C                   eval      cmzmbd = vlrhdl
061200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
061300000710     C                   if        prmfir = *blanks
061400050408     C                   eval      cmzfla = 'FIVAB00F'
061500050408     C                   eval      cmzmba = 'FIVAB00F'
061600000710     C                   else
061700000710     C                   eval      cmzfla = prmfir
061800000710     C                   eval      cmzmba = prmfir
061900000710     C                   endif
062000990920     C                   eval      cmznrr = *zeros
062100990920     C                   move      §ctrok        cmznrr
062200021018     C                   eval      cmzlba = vlrfl1
062300990920     C                   call(e)   'TIS711C'
062400990920     C                   parm                    dscmz
062500990921     C                   parm      *blanks       esito
062600990920     C                   if        %error
062700990920     C                             or cmzerr = '1'
062800990921     C                             or esito  = '1'
062900000710     C                   eval      wrkesito = '3'
063000990920     C                   endif
063100990920     C*
063200000613     C                   ENDSR
063300990910
063400010601
063500010601
063600010601
063700010601
063800010601      /TITLE Invio dei dati al punto operativo.
063900010601     C     opeini        BEGSR
064000010601     C*
064100010601     C* Inizializzo flag e contatori operativi
064200010601     C                   movel     '0'           flgGiro           1
064300010601     C                   movel     '0'           flgMulti          1
064400010601     C                   movel     '1'           flgStato          1
064500010615     C                   movel     '0'           flgOk             1
064600010601     C                   z-add     *zeros        cntNonEl         10 0
064700010601     C                   z-add     *zeros        depfil            3 0
064800010601     C                   z-add     *zeros        invfil            3 0
064900010601     C*
065000010601     C                   ENDSR
065100010601     C***
065200010601
065300010601
065400010601
065500010330
065600010330
065700000613     C     *inzsr        BEGSR
065800990910     C*
065900990910     C     *entry        plist
066000990920     C                   parm                    tivlrds
066100990921     C                   parm      wrkesito      esito
066200000724     C                   parm                    prmlit
066300000710     C                   parm                    prmfir
066400010330     C*
066500010330     C* CALCOLA LA DATA CORRENTE
066600010330     C                   time                    wn14             14 0
066700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
066800010330     C                   z-add     wn8           g08dat
066900010330     C                   z-add     *zeros        g08inv
067000010330     C                   movel     '0'           g08err
067100010330     C                   call      'XSRDA8'
067200010330     C                   parm                    wlbda8
067300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
067400000613     C*
067500000613     C                   ENDSR
067600000613     C***
067700990908
067800071022     Otitv1b5p  E            riepilogo         2
067900990915     O                                              'Upload via Internet'
068000990915     O                                           +1 'Traduzione TIVIN00R -'
068100050408     O                                           +1 'FIVABWWR'
068200990915     O                                           +1 'Testate bolle'
068300990915     O          E            riepilogo   2
068400990915     O                       wrkdata
068500990915     O                       wrkora              +1
068600990915     O                       procname            +1
068700990915     O          E            riepilogo   2
068800990915     O                                              'Cliente..................:'
068900010605     O                       VABCCM        z     +1
069000990915     O          E            riepilogo   2
069100990920     O                                              'Riferimento Strategi.....:'
069200990920     O                       vlrhdl              +1
069300990915     O          E            riepilogo   2
069400990915     O                                              'Giusti...................:'
069500971022     O                       §CTROK        2   +  1
069600990915     O          E            riepilogo   2
069700990915     O                                              'Sbagliati e corretti.....:'
069800971022     O                       §CTRMO        2   +  1
069900990915     O          E            riepilogo   2
070000990915     O                                              'Sbagliati e scartati.....:'
070100971022     O                       §CTRNO        2   +  1
070200000613
070300071022     Otitv1b5ps E            testdett          2
070400000613     O                                              'Upload via Internet'
070500000613     O                                           +1 'Traduzione TIVIN00R -'
070600050408     O                                           +1 'FIVABWWR'
070700000613     O                                           +1 'Testate bolle'
070800000616     O          E            testdett    3
070900000613     O                                           +2 'N° rec'
071000000613     O                                           +3 'Anteprima contenuto'
071100000616     O          E            rigadett    2
071200000613     O                       rrnum               +2
071300000621     O                       recko               +3
071400000616     O          E            findett     2
071500000613     O                       wrkdata
071600000613     O                       wrkora              +1
071700000613     O                       procname            +1
071800000616     O          E            findett     2
071900000613     O                                              'Cliente..................:'
072000010605     O                       VABCCM        z     +1
072100000616     O          E            findett     2
072200000613     O                                              'Riferimento Strategi.....:'
072300000613     O                       vlrhdl              +1
072400000616     O          E            findett     2
072500000613     O                                              'Giusti...................:'
072600000613     O                       §CTROK        2   +  1
072700000616     O          E            findett     2
072800000613     O                                              'Sbagliati e corretti.....:'
072900000613     O                       §CTRMO        2   +  1
073000000616     O          E            findett     2
073100000613     O                                              'Sbagliati e scartati.....:'
073200000613     O                       §CTRNO        2   +  1
073300000613** CMD - COMANDI CL
073400071022OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1B5P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
073500071022OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1B5PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
073600071022DLTOVR FILE(TITV1B5P TITV1B5PS) LVL(*)
073700010606
073800000613
