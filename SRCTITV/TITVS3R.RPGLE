000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700060316     Ftitvs3p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900060316     Ftitvs3ps  O    f  198        PRINTER usropn
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
002300060316     D tisi95ds      e ds
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
005100060316     C*
005200060316     C* Effettuo lancio TISI95 solo x chiusura
005300060316     C                   CLEAR                   TISI95DS
005400060316     C                   EVAL      I95TLA = 'C'
005500060316     C                   CALL      'TISI95R'
005600060316     C                   PARM                    TISI95DS
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
010000060316     C                   if        not %open(titvs3ps)
010100060316     C                   open      titvs3ps
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
011900060316     C                   if        %open(titvs3ps)
012000000616     C                   except    findett
012100060316     C                   close     titvs3ps
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
014000060316     C                   if        not %open(titvs3p)
014100060316     C                   open      titvs3p
014200990915     C                   endif
014300990915     C*
014400990915     C                   except    riepilogo
014500990915     C*
014600060316     C                   if        %open(titvs3p)
014700060316     C                   close     titvs3p
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
019200010601     C*
019300010330     C                   exsr      inzvar
019400010330     C                   exsr      defcam
019500010605     C                   exsr      impvab
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
021400020205     C*
021500050408     C* Scarico il buffer del FIVAB
021600050408     C  N31              WRITE     FIVAB000
021700010604     C*
021800020722     C                   endif
021900020722     C*
022000010604     C                   if        *in31 = *off and
022100010604     C                             *in32 = *off
022200010604     C                   eval      vinflg = '1'
022300010604     C                   else
022400010604     C                   eval      recko = vindta
022500010604     C                   except    rigadett
022600010604     C                   eval      vinflg = '2'
022700010604     C                   endif
022800010604     C                   endif
022900010604     C*
023000010330     C                   else
023100010330     C                   eval      vinflg = '1'
023200010330     C                   endif
023300010601     C*
023400010601     C  N70              update    tivin000
023500010330     C*
023600010330     C  N70              ENDdo
023700010601     C*
023800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023900010601     C                   if        cntNonEl = *zeros or
024000010601     C                             flgMulti = '0'
024100010330     C* Se non ci sono record con errori ...
024200010601     C                   if        §ctrno = 0 and
024300010604     C                             §ctrmo = 0 and
024400010601     C                             flgStato <> '2'
024500010330     C* ... restituisco esito OK.
024600010330     C                   eval      wrkesito = '0'
024700010330     C                   else
024800010330     C                   if        §ctrok > 0
024900010330     C                   eval      wrkesito = '1'
025000010330     C                   else
025100010615     C                   if        flgOk = '0'
025200010615     C                   eval      wrkesito = '2'
025300010615     C                   else
025400010615     C                   eval      wrkesito = '6'
025500010615     C                   endif
025600010330     C                   endif
025700010330     C                   endif
025800010601     C                   else
025900010601     C                   eval      wrkesito = '9'
026000010601     C                   endif
026100010330     C*
026200010330     C                   if        %open(tivin00r)
026300010330     C                   close     tivin00r
026400010330     C                   endif
026500050408     C                   if        %open(fivabwwr)
026600050408     C                   close     fivabwwr
026700010330     C                   endif
026800010601     C*
026900010601     C                   if        vlrpoi <> 999
027000010601     C                   eval      invfil = vlrpoi
027100010601     C                   endif
027200010330     C*
027300010330     C                   if        §ctrok > 0
027400010601     C                             and invfil > *zeros
027500010330     C                   exsr      invio
027600010330     C                   endif
027700010601     C*
027800010618     C                   if        flgGiro = '1'
027900010601     C                   exsr      endela
028000010618     C                   endif
028100010330     C*
028200010330     C                   ENDSR
028300010330     C***
028400010330
028500010601
028600010601
028700010601
028800010601
028900010330     C*----------------------------------------------------*
029000020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029100010330     C*----------------------------------------------------*
029200010330     C     INZVAR        BEGSR
029300010330     C*
029400020204     C                   Z-ADD     *zeros        Num5_0            5 0
029500020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029600020322     C                   MOVEL     '0'           FlgCAS            1
029700010330     C*
029800010330     C                   ENDSR
029900010330     C*----------------------------------------------------*
030000020722     C*  IMPOSTAZIONE CAMPI COSTANTI
030100010330     C*----------------------------------------------------*
030200010330     C     DEFCAM        BEGSR
030300010330     C*
030400020204     C* Inizializzo il buffer del record da scrivere
030500050408     C                   CLEAR                   FIVAB000
030600020204     C* Imposto i valori di default...
030700060316     C                   EVAL      VABCCM = 2020429
030800060316     C                   EVAL      VABLNP = 202
030900060105     C                   EVAL      VABCTR = 000
031000060316     C                   EVAL      VABTSP = 'C'
031100060303     C                   EVAL      VABCBO = '1'
031200020204     C* ... e poi verifico se sono stati passati come parametri
031300020204     C                   IF        vlrppt > *blanks
031400040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031600020204     C                   EXSR      CHKNUM
031700020204     C                   IF        PiInt=*on
031800020204     C                   Z-ADD     PiVal         VABCCM
031900020204     C                   ENDIF
032000040301     C                   ENDIF
032100040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
032200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032300020204     C                   EXSR      CHKNUM
032400020204     C                   IF        PiInt=*on
032500020204     C                   Z-ADD     PiVal         VABLNP
032600020204     C                   ENDIF
032700040301     C                   ENDIF
032800040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
032900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033000020204     C                   EXSR      CHKNUM
033100020204     C                   IF        PiInt=*on
033200020204     C                   Z-ADD     PiVal         VABCTR
033300040301     C                   ENDIF
033400020204     C                   ENDIF
033500020204     C                   ENDIF
033600020204     C*
033700010330     C                   ENDSR
033800010607     C*----------------------------------------------------*
033900050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034000010607     C*----------------------------------------------------*
034100010607     C     IMPVAB        BEGSR
034200010607     C*
034300010607     C                   SETOFF                                       3132
034400010607     C*
034500010607     C* Reperimento campi ALFA
034600010607     C*
034700010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034800060316     C                   EVAL      VABRSD=%trim(%subst(vindta:88:34))
034900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035100020117     C* ==
035200060316     C                   EVAL      VABIND=%trim(%subst(vindta:143:35))
035300060316     C                   EVAL      VABLOD=%trim(%subst(vindta:178:35))
035400060316     C                   EVAL      VABPRD=%trim(%subst(vindta:213:2))
035500060316     C                   EVAL      VABRMA=%trim(%subst(vindta:36:6))
035600060612     C                   EVAL      VABRD2=%trim(%subst(vindta:123:20))
035700060612     C                   EVAL      VABNOT=%trim(%subst(vindta:281:35))
035800060612     C                   EVAL      VABNT2=%trim(%subst(vindta:281+35:35))
035900060316     C                   IF        %trim(%subst(vindta:236:10)) = 'FRANCO ADD'
036000060316     C                   EVAL      VABCBO='1'
036100060316     C                   ELSE
036200060316     C                   EVAL      VABCBO='2'
036300060316     C                   ENDIF
036400010607     C*
036500010607     C* Reperimento campi NUMERICI
036600020722     C                   MOVEL     DATCOR        VABAAS
036700020722     C                   MOVE      DATCOR        VABMGS
036800060105     C* RMN/NSP
036900060316     C                   EVAL      PiStr=%trim(%subst(vindta:36:6))
037000050408     C                   EXSR      CHKNUM
037100050408     C                   IF        PiInt=*on
037200050408     C                   Z-ADD     PiVal         VABRMN
037300050908     C                   Z-ADD     PiVal         VABNSP
037400050408     C                   ELSE
037500051219     C                   SETON                                        31
037600050408     C                   Z-ADD     *zeros        VABRMN
037700050408     C                   EVAL      vinmsg = %trimr(vinmsg)
037800050908     C                             + ' ' + 'VABRMN VABNSP'
037900050408     C                   ENDIF
038000040420     C* CAD
038100060316     C                   EVAL      PiStr=%trim(%subst(vindta:223:5))
038200010607     C                   EXSR      CHKNUM
038300010607     C                   IF        PiInt=*on
038400020204     C                   Z-ADD     PiVal         Num5_0
038500020204     C                   MOVEL(p)  Num5_0        VABCAD
038600010607     C                   ELSE
038700010607     C                   SETON                                        32
038800020204     C                   EVAL      VABCAD = *zeros
038900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039000020204     C                             + ' ' + 'VABCAD'
039100010607     C                   ENDIF
039200060316     C* Reperisco la provincia dal CAP e dalla località
039300060316     C                   IF        VABCAD <> *blanks AND
039400060316     C                             VABPRD  = *blanks
039500060316     C                   CLEAR                   TISI95DS
039600060316     C                   EVAL      I95TCN = '3'
039700060316     C                   Z-ADD     datcor        I95DAT
039800060316     C                   EVAL      I95CAP = VABCAD
039900060316     C                   EVAL      I95LOC = VABLOD
040000060316     C                   EVAL      I95NAR = VABNZD
040100060316     C                   CALL      'TISI95R'
040200060316     C                   PARM                    TISI95DS
040300060316     C                   EVAL      VABPRD = O95PRV
040400060316     C                   ENDIF
040500040420     C* NCL
040600060316     C                   EVAL      PiStr=%trim(%subst(vindta:53:5))
040700010607     C                   EXSR      CHKNUM
040800010607     C                   IF        PiInt=*on
040900010607     C                   Z-ADD     PiVal         VABNCL
041000010607     C                   ELSE
041100010607     C                   SETON                                        32
041200010607     C                   Z-ADD     *zeros        VABNCL
041300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041400010607     C                             + ' ' + 'VABNCL'
041500010607     C                   ENDIF
041600040420     C* PKB
041700060316     C                   EVAL      PiStr=%trim(%subst(vindta:46:7))
041800010607     C                   EXSR      CHKNUM
041900010607     C                   IF        PiNum=*on
042000060316     C                   EVAL(H)   VABPKB=PiVal/100                             * gestisco 2 dec.
042100010607     C                   ELSE
042200010607     C                   SETON                                        32
042300010607     C                   Z-ADD     *zeros        VABPKB
042400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042500010607     C                             + ' ' + 'VABPKB'
042600010607     C                   ENDIF
042700060421     C* CAS
042800060421     C                   IF        %subst(vindta:266:15)<>*zeros
042900060421     C                   EVAL      FlgCAS='1'
043000060421     C                   ENDIF
043100060421     C                   EVAL      PiStr=%trim(%subst(vindta:266:15))
043200060421     C                   EXSR      CHKNUM
043300060421     C                   IF        PiNum=*on
043400060421     C                   EVAL      PiVal=PiVal/100000                           * gestisco 5 dec.
043500060421     C                   Z-ADD(H)  PiVal         VABCAS
043600060421     C                   EVAL      VABVCA='EUR'
043700060421     C                   ELSE
043800060421     C                   SETON                                        32
043900060421     C                   Z-ADD     *zeros        VABCAS
044000060421     C                   EVAL      vinmsg = %trimr(vinmsg)
044100060421     C                             + ' ' + 'VABCAS'
044200060421     C                   ENDIF
044300020322     C*
044400020322     C* Considerazioni finali su CBO/CAS
044500050908     C                   IF        FlgCAS = '1'
044600040713     C                   IF        VABCBO = '1'
044700020322     C                   EVAL      VABCBO = '4'
044800040713     C                   ENDIF
044900040713     C                   IF        VABCBO = '2'
045000040713     C                   EVAL      VABCBO = '6'
045100040713     C                   ENDIF
045200020322     C                   ENDIF
045300020204     C*
045400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045500020204     C                   EXSR      CHKIMPDIV
045600020204     C*
045700010607     C                   ENDSR
045800010607     C*----------------------------------------------------*
045900010601
046000020204
046100020204     C*----------------------------------------------------*
046200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046300020204     C*----------------------------------------------------*
046400020204     C     CHKIMPDIV     BEGSR
046500020204     C*
046600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046700020204     C                   Z-ADD     *zeros        wrkDec            9 9
046800020204     C*
046900020204     C* Come prima cosa effettuo considerazioni sulla divisa
047000020204     C                   IF        vabIAS > *zeros
047100020204     C                   IF        vabVAS <> 'EUR'
047200020204     C                   EVAL      vabVAS =  'ITL'
047300020204     C                   ENDIF
047400020204     C                   ENDIF
047500020204     C*
047600020204     C                   IF        vabCAS > *zeros
047700020204     C                   IF        vabVCA <> 'EUR'
047800020204     C                   EVAL      vabVCA =  'ITL'
047900020204     C                   ENDIF
048000020204     C                   ENDIF
048100020204     C*
048200020204     C                   IF        vabVMD > *zeros
048300020321     C                   IF        vabVAD <> 'EUR'
048400020204     C                   EVAL      vabVAD =  'ITL'
048500020204     C                   ENDIF
048600020204     C                   ENDIF
048700020204     C*
048800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048900020204     C                   Z-ADD     vabIAS        wrkDec
049000020204     C                   IF        wrkDec > *zeros
049100020204     C                   IF        vabVAS = 'ITL'
049200020204     C                   EVAL      vabIAS = *zeros
049300020204     C                   ENDIF
049400020204     C                   ENDIF
049500020204     C*
049600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
049700020204     C                   Z-ADD     vabCAS        wrkDec
049800020204     C                   IF        wrkDec > *zeros
049900020204     C                   IF        vabVCA = 'ITL'
050000020204     C                   EVAL      vabCAS = *zeros
050100020204     C                   ENDIF
050200020204     C                   ENDIF
050300020204     C*
050400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050500020204     C                   Z-ADD     vabVMD        wrkDec
050600020204     C                   IF        wrkDec > *zeros
050700020204     C                   IF        vabVAD = 'ITL'
050800020204     C                   EVAL      vabVMD = *zeros
050900020204     C                   ENDIF
051000020204     C                   ENDIF
051100020204     C*
051200020204     C                   ENDSR
051300020204     C***
051400020204
051500010330
051600010330
051700010330     C*----------------------------------------------------*
051800010330     C*  CONTROLLO NUMERICITA' CAMPI
051900010330     C*----------------------------------------------------*
052000010330     C     CHKNUM        BEGSR
052100010330     C*
052200010606     C                   IF        PiDecChr = *blanks
052300010606     C                   EVAL      PiDecChr = ','
052400010606     C                   ENDIF
052500010606     C*
052600010606     C                   CALL(e)   'ISNUMERIC'
052700010330     C                   PARM                    PiStr            30
052800010606     C                   PARM                    PiDecChr          1
052900010330     C                   PARM      *ZEROS        PiVal            30 9
053000010330     C                   PARM      '0'           PiInt             1
053100010330     C                   PARM      '0'           PiNum             1
053200010330     C                   IF        %error
053300010606     C                   EVAL      PiNum=*off
053400010330     C                   ENDIF
053500010330     C*
053600010330     C                   ENDSR
053700010330     C***
053800010330
053900010601
054000010601
054100010601
054200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
054300010601     C     repfil        BEGSR
054400010601     C*
054500010601     C                   if        invfil = *zeros and
054600010601     C                             depfil > *zeros and
054700010629     C                             (vinflg = *blanks or
054800010629     C                              vinflg = *zeros)
054900010601     C
055000010601     C                   eval      invfil = depfil
055100010601     C                   endif
055200010601     C*
055300010601     C                   if        depfil <> invfil and
055400010601     C                             invfil > *zeros
055500010601     C                   eval      flgMulti = '1'
055600010601     C                   if        vinflg = *blanks
055700010601     C                   add       1             cntNonEl
055800010601     C                   endif
055900010601     C                   endif
056000010601     C*
056100010601     C                   if        vinflg = '2'
056200010601     C                   eval      flgStato = '2'
056300010601     C                   endif
056400010601     C*
056500010601     C                   ENDSR
056600010601     C***
056700010601
056800010601
056900010601
057000010330
057100010330
057200010330
057300990920      /TITLE Invio dei dati al punto operativo.
057400000613     C     invio         BEGSR
057500990920     C*
057600990920     C                   reset                   dscmz
057700010601     C                   move      invfil        cmzdst
057800990920     C                   eval      cmzfld = vlrfou
057900990920     C                   eval      cmzmbd = vlrhdl
058000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
058100000710     C                   if        prmfir = *blanks
058200050408     C                   eval      cmzfla = 'FIVAB00F'
058300050408     C                   eval      cmzmba = 'FIVAB00F'
058400000710     C                   else
058500000710     C                   eval      cmzfla = prmfir
058600000710     C                   eval      cmzmba = prmfir
058700000710     C                   endif
058800990920     C                   eval      cmznrr = *zeros
058900990920     C                   move      §ctrok        cmznrr
059000021018     C                   eval      cmzlba = vlrfl1
059100990920     C                   call(e)   'TIS711C'
059200990920     C                   parm                    dscmz
059300990921     C                   parm      *blanks       esito
059400990920     C                   if        %error
059500990920     C                             or cmzerr = '1'
059600990921     C                             or esito  = '1'
059700000710     C                   eval      wrkesito = '3'
059800990920     C                   endif
059900990920     C*
060000000613     C                   ENDSR
060100990910
060200010601
060300010601
060400010601
060500010601
060600010601      /TITLE Invio dei dati al punto operativo.
060700010601     C     opeini        BEGSR
060800010601     C*
060900010601     C* Inizializzo flag e contatori operativi
061000010601     C                   movel     '0'           flgGiro           1
061100010601     C                   movel     '0'           flgMulti          1
061200010601     C                   movel     '1'           flgStato          1
061300010615     C                   movel     '0'           flgOk             1
061400010601     C                   z-add     *zeros        cntNonEl         10 0
061500010601     C                   z-add     *zeros        depfil            3 0
061600010601     C                   z-add     *zeros        invfil            3 0
061700010601     C*
061800010601     C                   ENDSR
061900010601     C***
062000010601
062100010601
062200010601
062300010330
062400010330
062500000613     C     *inzsr        BEGSR
062600990910     C*
062700990910     C     *entry        plist
062800990920     C                   parm                    tivlrds
062900990921     C                   parm      wrkesito      esito
063000000724     C                   parm                    prmlit
063100000710     C                   parm                    prmfir
063200010330     C*
063300010330     C* CALCOLA LA DATA CORRENTE
063400010330     C                   time                    wn14             14 0
063500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063600010330     C                   z-add     wn8           g08dat
063700010330     C                   z-add     *zeros        g08inv
063800010330     C                   movel     '0'           g08err
063900010330     C                   call      'XSRDA8'
064000010330     C                   parm                    wlbda8
064100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
064200000613     C*
064300000613     C                   ENDSR
064400000613     C***
064500990908
064600060316     Otitvs3p   E            riepilogo         2
064700990915     O                                              'Upload via Internet'
064800990915     O                                           +1 'Traduzione TIVIN00R -'
064900050408     O                                           +1 'FIVABWWR'
065000990915     O                                           +1 'Testate bolle'
065100990915     O          E            riepilogo   2
065200990915     O                       wrkdata
065300990915     O                       wrkora              +1
065400990915     O                       procname            +1
065500990915     O          E            riepilogo   2
065600990915     O                                              'Cliente..................:'
065700010605     O                       VABCCM        z     +1
065800990915     O          E            riepilogo   2
065900990920     O                                              'Riferimento Strategi.....:'
066000990920     O                       vlrhdl              +1
066100990915     O          E            riepilogo   2
066200990915     O                                              'Giusti...................:'
066300971022     O                       §CTROK        2   +  1
066400990915     O          E            riepilogo   2
066500990915     O                                              'Sbagliati e corretti.....:'
066600971022     O                       §CTRMO        2   +  1
066700990915     O          E            riepilogo   2
066800990915     O                                              'Sbagliati e scartati.....:'
066900971022     O                       §CTRNO        2   +  1
067000000613
067100060316     Otitvs3ps  E            testdett          2
067200000613     O                                              'Upload via Internet'
067300000613     O                                           +1 'Traduzione TIVIN00R -'
067400050408     O                                           +1 'FIVABWWR'
067500000613     O                                           +1 'Testate bolle'
067600000616     O          E            testdett    3
067700000613     O                                           +2 'N° rec'
067800000613     O                                           +3 'Anteprima contenuto'
067900000616     O          E            rigadett    2
068000000613     O                       rrnum               +2
068100000621     O                       recko               +3
068200000616     O          E            findett     2
068300000613     O                       wrkdata
068400000613     O                       wrkora              +1
068500000613     O                       procname            +1
068600000616     O          E            findett     2
068700000613     O                                              'Cliente..................:'
068800010605     O                       VABCCM        z     +1
068900000616     O          E            findett     2
069000000613     O                                              'Riferimento Strategi.....:'
069100000613     O                       vlrhdl              +1
069200000616     O          E            findett     2
069300000613     O                                              'Giusti...................:'
069400000613     O                       §CTROK        2   +  1
069500000616     O          E            findett     2
069600000613     O                                              'Sbagliati e corretti.....:'
069700000613     O                       §CTRMO        2   +  1
069800000616     O          E            findett     2
069900000613     O                                              'Sbagliati e scartati.....:'
070000000613     O                       §CTRNO        2   +  1
070100000613** CMD - COMANDI CL
070200060316OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVS3P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
070300060316OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVS3PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
070400060316DLTOVR FILE(TITVS3P TITVS3PS) LVL(*)
070500010606
070600000613
