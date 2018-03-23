000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700071029     Ftitv1b7p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900071029     Ftitv1b7ps O    f  198        PRINTER usropn
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
010000071029     C                   if        not %open(titv1b7ps)
010100071029     C                   open      titv1b7ps
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
011900071029     C                   if        %open(titv1b7ps)
012000000616     C                   except    findett
012100071029     C                   close     titv1b7ps
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
014000071029     C                   if        not %open(titv1b7p)
014100071029     C                   open      titv1b7p
014200990915     C                   endif
014300990915     C*
014400990915     C                   except    riepilogo
014500990915     C*
014600071029     C                   if        %open(titv1b7p)
014700071029     C                   close     titv1b7p
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
031200071029     C                   EVAL      VABCCM = 0894032
031300071029     C                   EVAL      VABLNP = 089
031400071029     C                   EVAL      VABCTR = 001
031500120220     C                   EVAL      VABCTM = '7T'
031600070208     C                   EVAL      VABCBO = '1'
031700071029     C                   EVAL      VABTSP = 'C'
031800020204     C* ... e poi verifico se sono stati passati come parametri
031900020204     C                   IF        vlrppt > *blanks
032000040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
032100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032200020204     C                   EXSR      CHKNUM
032300020204     C                   IF        PiInt=*on
032400020204     C                   Z-ADD     PiVal         VABCCM
032500020204     C                   ENDIF
032600040301     C                   ENDIF
032700040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
032800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032900020204     C                   EXSR      CHKNUM
033000020204     C                   IF        PiInt=*on
033100020204     C                   Z-ADD     PiVal         VABLNP
033200020204     C                   ENDIF
033300040301     C                   ENDIF
033400040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
033500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033600020204     C                   EXSR      CHKNUM
033700020204     C                   IF        PiInt=*on
033800020204     C                   Z-ADD     PiVal         VABCTR
033900040301     C                   ENDIF
034000020204     C                   ENDIF
034100020204     C                   ENDIF
034200020204     C*
034300010330     C                   ENDSR
034400010607     C*----------------------------------------------------*
034500050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034600010607     C*----------------------------------------------------*
034700010607     C     IMPVAB        BEGSR
034800010607     C*
034900010607     C                   SETOFF                                       3132
035000010607     C*
035100010607     C* Reperimento campi ALFA
035200010607     C*
035300010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
035400071029     C                   EVAL      VABRSD=%trim(%subst(vindta:208:35))
035500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035700020117     C* ==
035800071029     C                   EVAL      VABRD2=%trim(%subst(vindta:208+35:50-35))
035900071029     C                   EVAL      VABIND=%trim(%subst(vindta:258:50))
036000071029     C                   EVAL      VABLOD=%trim(%subst(vindta:318:50))
036100071029     C                   EVAL      VABPRD=%trim(%subst(vindta:368:2))
036200071029     C                   EVAL      VABRMA=%trim(%subst(vindta:33:10))
036300071029     C                   EVAL      VABNOT=%trim(%subst(vindta:388:35))
036400071029     C                   EVAL      VABNT2=%trim(%subst(vindta:388+35:35))
036500091007     C                   EVAL      VABTIC=%trim(%subst(vindta:530:2))
036600060510     C*
036700060510     C* Reperimento campi NUMERICI
036800060510     C                   MOVEL     DATCOR        VABAAS
036900060510     C                   MOVE      DATCOR        VABMGS
037000060710     C* NSP/RMN
037100071029     C                   EVAL      PiStr=%trim(%subst(vindta:6:7))
037200060510     C                   EXSR      CHKNUM
037300060510     C                   IF        PiInt=*on
037400060510     C                   Z-ADD     PiVal         VABNSP
037500060510     C                   Z-ADD     PiVal         VABRMN
037600060404     C                   ELSE
037700070502     C                   SETON                                        31
037800060510     C                   Z-ADD     *zeros        VABNSP
037900060510     C                   Z-ADD     1             VABRMN
038000060404     C                   EVAL      vinmsg = %trimr(vinmsg)
038100060510     C                             + ' ' + 'VABNSP VABRMN'
038200060404     C                   ENDIF
038300040420     C* CAD
038400071029     C                   EVAL      PiStr=%trim(%subst(vindta:308:5))
038500010607     C                   EXSR      CHKNUM
038600010607     C                   IF        PiInt=*on
038700020204     C                   Z-ADD     PiVal         Num5_0
038800020204     C                   MOVEL(p)  Num5_0        VABCAD
038900010607     C                   ELSE
039000010607     C                   SETON                                        32
039100020204     C                   EVAL      VABCAD = *zeros
039200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039300020204     C                             + ' ' + 'VABCAD'
039400060316     C                   ENDIF
039500070502     C* Reperisco la provincia dal CAP e dalla località
039600070502     C                   IF        VABCAD <> *blanks AND
039700070502     C                             VABPRD  = *blanks
039800070502     C                   CLEAR                   TISI95DS
039900070502     C                   EVAL      I95TCN = '3'
040000070502     C                   Z-ADD     datcor        I95DAT
040100070502     C                   EVAL      I95CAP = VABCAD
040200070502     C                   EVAL      I95LOC = VABLOD
040300070502     C                   EVAL      I95NAR = VABNZD
040400070502     C                   CALL      'TISI95R'
040500070502     C                   PARM                    TISI95DS
040600070502     C                   EVAL      VABPRD = O95PRV
040700070502     C                   ENDIF
040800040420     C* NCL
040900071029     C                   EVAL      PiStr=%trim(%subst(vindta:373:5))
041000010607     C                   EXSR      CHKNUM
041100010607     C                   IF        PiInt=*on
041200070606     C                   Z-ADD     PiVal         VABNCL
041300010607     C                   ELSE
041400010607     C                   SETON                                        32
041500010607     C                   Z-ADD     *zeros        VABNCL
041600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041700010607     C                             + ' ' + 'VABNCL'
041800010607     C                   ENDIF
041900071029     C* PKB
042000071029     C                   EVAL      PiStr=%trim(%subst(vindta:378:10))
042100071029     C                   EVAL      PiDecChr = '.'
042200010607     C                   EXSR      CHKNUM
042300071029     C                   EVAL      PiDecChr = *blanks
042400010607     C                   IF        PiNum=*on
042500071003     C                   Z-ADD(H)  PiVal         VABPKB
042600010607     C                   ELSE
042700010607     C                   SETON                                        32
042800010607     C                   Z-ADD     *zeros        VABPKB
042900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043000010607     C                             + ' ' + 'VABPKB'
043100010607     C                   ENDIF
043200071022     C* CAS
043300071029     C                   IF        %trim(%subst(vindta:518:10)) <> *blanks AND
043400071029     C                             %trim(%subst(vindta:518:10)) <> *zeros  AND
043500071029     C                             %trim(%subst(vindta:518:10)) <> '0'
043600071022     C                   EVAL      FlgCAS = '1'
043700071029     C                   EVAL      PiStr=%trim(%subst(vindta:518:10))
043800071029     C                   EVAL      PiDecChr = ','
043900071022     C                   EXSR      CHKNUM
044000071029     C                   EVAL      PiDecChr = *blanks
044100071022     C                   IF        PiNum=*on
044200071022     C                   Z-ADD(H)  PiVal         VABCAS
044300071022     C                   EVAL      VABVCA ='EUR'
044400071022     C                   ELSE
044500071022     C                   SETON                                        32
044600071022     C                   Z-ADD     *zeros        VABCAS
044700071022     C                   EVAL      vinmsg = %trimr(vinmsg)
044800071022     C                             + ' ' + 'VABCAS'
044900071022     C                   ENDIF
045000071022     C                   ENDIF
045100020322     C*
045200020322     C* Considerazioni finali su CBO/CAS
045300050908     C                   IF        FlgCAS = '1'
045400040713     C                   IF        VABCBO = '1'
045500020322     C                   EVAL      VABCBO = '4'
045600040713     C                   ENDIF
045700040713     C                   IF        VABCBO = '2'
045800040713     C                   EVAL      VABCBO = '6'
045900040713     C                   ENDIF
046000020322     C                   ENDIF
046100020204     C*
046200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046300020204     C                   EXSR      CHKIMPDIV
046400020204     C*
046500010607     C                   ENDSR
046600010607     C*----------------------------------------------------*
046700010601
046800020204
046900020204     C*----------------------------------------------------*
047000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
047100020204     C*----------------------------------------------------*
047200020204     C     CHKIMPDIV     BEGSR
047300020204     C*
047400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
047500020204     C                   Z-ADD     *zeros        wrkDec            9 9
047600020204     C*
047700020204     C* Come prima cosa effettuo considerazioni sulla divisa
047800020204     C                   IF        vabIAS > *zeros
047900020204     C                   IF        vabVAS <> 'EUR'
048000020204     C                   EVAL      vabVAS =  'ITL'
048100020204     C                   ENDIF
048200020204     C                   ENDIF
048300020204     C*
048400020204     C                   IF        vabCAS > *zeros
048500020204     C                   IF        vabVCA <> 'EUR'
048600020204     C                   EVAL      vabVCA =  'ITL'
048700020204     C                   ENDIF
048800020204     C                   ENDIF
048900020204     C*
049000020204     C                   IF        vabVMD > *zeros
049100020321     C                   IF        vabVAD <> 'EUR'
049200020204     C                   EVAL      vabVAD =  'ITL'
049300020204     C                   ENDIF
049400020204     C                   ENDIF
049500020204     C*
049600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049700020204     C                   Z-ADD     vabIAS        wrkDec
049800020204     C                   IF        wrkDec > *zeros
049900020204     C                   IF        vabVAS = 'ITL'
050000020204     C                   EVAL      vabIAS = *zeros
050100020204     C                   ENDIF
050200020204     C                   ENDIF
050300020204     C*
050400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
050500020204     C                   Z-ADD     vabCAS        wrkDec
050600020204     C                   IF        wrkDec > *zeros
050700020204     C                   IF        vabVCA = 'ITL'
050800020204     C                   EVAL      vabCAS = *zeros
050900020204     C                   ENDIF
051000020204     C                   ENDIF
051100020204     C*
051200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
051300020204     C                   Z-ADD     vabVMD        wrkDec
051400020204     C                   IF        wrkDec > *zeros
051500020204     C                   IF        vabVAD = 'ITL'
051600020204     C                   EVAL      vabVMD = *zeros
051700020204     C                   ENDIF
051800020204     C                   ENDIF
051900020204     C*
052000020204     C                   ENDSR
052100020204     C***
052200020204
052300010330
052400010330
052500010330     C*----------------------------------------------------*
052600010330     C*  CONTROLLO NUMERICITA' CAMPI
052700010330     C*----------------------------------------------------*
052800010330     C     CHKNUM        BEGSR
052900010330     C*
053000010606     C                   IF        PiDecChr = *blanks
053100070502     C                   EVAL      PiDecChr = '.'
053200010606     C                   ENDIF
053300010606     C*
053400010606     C                   CALL(e)   'ISNUMERIC'
053500010330     C                   PARM                    PiStr            30
053600010606     C                   PARM                    PiDecChr          1
053700010330     C                   PARM      *ZEROS        PiVal            30 9
053800010330     C                   PARM      '0'           PiInt             1
053900010330     C                   PARM      '0'           PiNum             1
054000010330     C                   IF        %error
054100010606     C                   EVAL      PiNum=*off
054200010330     C                   ENDIF
054300010330     C*
054400010330     C                   ENDSR
054500010330     C***
054600010330
054700010601
054800010601
054900010601
055000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
055100010601     C     repfil        BEGSR
055200010601     C*
055300010601     C                   if        invfil = *zeros and
055400010601     C                             depfil > *zeros and
055500010629     C                             (vinflg = *blanks or
055600010629     C                              vinflg = *zeros)
055700010601     C
055800010601     C                   eval      invfil = depfil
055900010601     C                   endif
056000010601     C*
056100010601     C                   if        depfil <> invfil and
056200010601     C                             invfil > *zeros
056300010601     C                   eval      flgMulti = '1'
056400010601     C                   if        vinflg = *blanks
056500010601     C                   add       1             cntNonEl
056600010601     C                   endif
056700010601     C                   endif
056800010601     C*
056900010601     C                   if        vinflg = '2'
057000010601     C                   eval      flgStato = '2'
057100010601     C                   endif
057200010601     C*
057300010601     C                   ENDSR
057400010601     C***
057500010601
057600010601
057700010601
057800010330
057900010330
058000010330
058100990920      /TITLE Invio dei dati al punto operativo.
058200000613     C     invio         BEGSR
058300990920     C*
058400990920     C                   reset                   dscmz
058500010601     C                   move      invfil        cmzdst
058600990920     C                   eval      cmzfld = vlrfou
058700990920     C                   eval      cmzmbd = vlrhdl
058800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
058900000710     C                   if        prmfir = *blanks
059000050408     C                   eval      cmzfla = 'FIVAB00F'
059100050408     C                   eval      cmzmba = 'FIVAB00F'
059200000710     C                   else
059300000710     C                   eval      cmzfla = prmfir
059400000710     C                   eval      cmzmba = prmfir
059500000710     C                   endif
059600990920     C                   eval      cmznrr = *zeros
059700990920     C                   move      §ctrok        cmznrr
059800021018     C                   eval      cmzlba = vlrfl1
059900990920     C                   call(e)   'TIS711C'
060000990920     C                   parm                    dscmz
060100990921     C                   parm      *blanks       esito
060200990920     C                   if        %error
060300990920     C                             or cmzerr = '1'
060400990921     C                             or esito  = '1'
060500000710     C                   eval      wrkesito = '3'
060600990920     C                   endif
060700990920     C*
060800000613     C                   ENDSR
060900990910
061000010601
061100010601
061200010601
061300010601
061400010601      /TITLE Invio dei dati al punto operativo.
061500010601     C     opeini        BEGSR
061600010601     C*
061700010601     C* Inizializzo flag e contatori operativi
061800010601     C                   movel     '0'           flgGiro           1
061900010601     C                   movel     '0'           flgMulti          1
062000010601     C                   movel     '1'           flgStato          1
062100010615     C                   movel     '0'           flgOk             1
062200010601     C                   z-add     *zeros        cntNonEl         10 0
062300010601     C                   z-add     *zeros        depfil            3 0
062400010601     C                   z-add     *zeros        invfil            3 0
062500010601     C*
062600010601     C                   ENDSR
062700010601     C***
062800010601
062900010601
063000010601
063100010330
063200010330
063300000613     C     *inzsr        BEGSR
063400990910     C*
063500990910     C     *entry        plist
063600990920     C                   parm                    tivlrds
063700990921     C                   parm      wrkesito      esito
063800000724     C                   parm                    prmlit
063900000710     C                   parm                    prmfir
064000010330     C*
064100010330     C* CALCOLA LA DATA CORRENTE
064200010330     C                   time                    wn14             14 0
064300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064400010330     C                   z-add     wn8           g08dat
064500010330     C                   z-add     *zeros        g08inv
064600010330     C                   movel     '0'           g08err
064700010330     C                   call      'XSRDA8'
064800010330     C                   parm                    wlbda8
064900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
065000000613     C*
065100000613     C                   ENDSR
065200000613     C***
065300990908
065400071029     Otitv1b7p  E            riepilogo         2
065500990915     O                                              'Upload via Internet'
065600990915     O                                           +1 'Traduzione TIVIN00R -'
065700050408     O                                           +1 'FIVABWWR'
065800990915     O                                           +1 'Testate bolle'
065900990915     O          E            riepilogo   2
066000990915     O                       wrkdata
066100990915     O                       wrkora              +1
066200990915     O                       procname            +1
066300990915     O          E            riepilogo   2
066400990915     O                                              'Cliente..................:'
066500010605     O                       VABCCM        z     +1
066600990915     O          E            riepilogo   2
066700990920     O                                              'Riferimento Strategi.....:'
066800990920     O                       vlrhdl              +1
066900990915     O          E            riepilogo   2
067000990915     O                                              'Giusti...................:'
067100971022     O                       §CTROK        2   +  1
067200990915     O          E            riepilogo   2
067300990915     O                                              'Sbagliati e corretti.....:'
067400971022     O                       §CTRMO        2   +  1
067500990915     O          E            riepilogo   2
067600990915     O                                              'Sbagliati e scartati.....:'
067700971022     O                       §CTRNO        2   +  1
067800000613
067900071029     Otitv1b7ps E            testdett          2
068000000613     O                                              'Upload via Internet'
068100000613     O                                           +1 'Traduzione TIVIN00R -'
068200050408     O                                           +1 'FIVABWWR'
068300000613     O                                           +1 'Testate bolle'
068400000616     O          E            testdett    3
068500000613     O                                           +2 'N° rec'
068600000613     O                                           +3 'Anteprima contenuto'
068700000616     O          E            rigadett    2
068800000613     O                       rrnum               +2
068900000621     O                       recko               +3
069000000616     O          E            findett     2
069100000613     O                       wrkdata
069200000613     O                       wrkora              +1
069300000613     O                       procname            +1
069400000616     O          E            findett     2
069500000613     O                                              'Cliente..................:'
069600010605     O                       VABCCM        z     +1
069700000616     O          E            findett     2
069800000613     O                                              'Riferimento Strategi.....:'
069900000613     O                       vlrhdl              +1
070000000616     O          E            findett     2
070100000613     O                                              'Giusti...................:'
070200000613     O                       §CTROK        2   +  1
070300000616     O          E            findett     2
070400000613     O                                              'Sbagliati e corretti.....:'
070500000613     O                       §CTRMO        2   +  1
070600000616     O          E            findett     2
070700000613     O                                              'Sbagliati e scartati.....:'
070800000613     O                       §CTRNO        2   +  1
070900000613** CMD - COMANDI CL
071000071029OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1B7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
071100071029OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1B7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
071200071029DLTOVR FILE(TITV1B7P TITV1B7PS) LVL(*)
071300010606
071400000613
