000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400100525     Ftabel00f  if   e           k disk
000500000724     Fazorg01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700050408     FFIVABWWR  O    E             DISK    usropn
000800060316     Ftitvs2p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000060316     Ftitvs2ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200000313     D*
001300000613     D*------------
001400000613     D* COMANDI
001500000613     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700010330     D*----------------------------------------------------
001800010330     D* DICHIARAZIOINE VARIABILI DI WRK
001900010330     D*----------------------------------------------------
002000010330     D dscmz         e ds                  inz
002100010330     D psds           sds
002200010330     D  procname         *PROC
002300010330     D tivlrds       e ds                  extname(tivlr00f)
002400060316     D tisi95ds      e ds
002500100525     D ds15          e ds
002600010330     D esito           s              1
002700010330     D prmlit          s             10
002800010330     D prmfir          s             10
002900010330     D wrkesito        s                   like(esito)
003000010330     D wrkdata         s               d
003100010330     D wrkora          s               t
003200010330     D rrnum           s              6  0 INZ(*zeros)
003300010330     D recko           s            150    INZ(*blanks)
003400011113     D depcmd          s            150    INZ(*blanks)
003500100525
003600100525     D*------------
003700100525     D* SCHIERE
003800100525     D*------------
003900100525     D skNAZISO        S              3    DIM(1000)
004000100525     D skNAZBAR        S              3    DIM(1000)
004100100525     D jNAZ            s              5  0 INZ(*zeros)
004200100525
004300010330     D*------------------
004400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500010330     D*------------------
004600010330     D WLBDA8          DS                  INZ
004700010330     D  G08DAT                 1      8  0
004800010330     D  G08INV                 9     16  0
004900010330     D  G08ERR                17     17
005000010330     D  G08TGI                18     22  0
005100010330
005200010330
005300100525     C*
005400100525     C* Carico le tabelle
005500100525     C                   exsr      cartab
005600100525     C*
005700990915     C                   time                    wrkdata
005800990915     C                   time                    wrkora
005900000913     C                   reset                   rrnum
006000990921     C                   reset                   esito
006100990921     C                   reset                   wrkesito
006200010601     C*
006300010601     C                   exsr      opeini
006400010605     C                   exsr      rwvab
006500060316     C*
006600060316     C* Effettuo lancio TISI95 solo x chiusura
006700060316     C                   CLEAR                   TISI95DS
006800060316     C                   EVAL      I95TLA = 'C'
006900060316     C                   CALL      'TISI95R'
007000060316     C                   PARM                    TISI95DS
007100010601     C*
007200010601     C                   seton                                        lr
007300010601
007400010601
007500010601
007600010601
007700010601     C*--------------------------------------------------------
007800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007900010601     C*--------------------------------------------------------
008000010601     C     PREELA        BEGSR
008100010601     C*
008200010601     C* SE OCCORRE SPEDIRE IN FILIALE
008300010601     C                   if        invfil <> *zeros and
008400010601     C                             flgGiro = '0'
008500010601     C*
008600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008700010601     C                   eval      flgGiro = '1'
008800000724     C*
008900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009100010601     C     invfil        chain     azorg01l
009200000724     C                   if        %found
009300000724     C                   movel(p)  CMD(1)        depcmd
009400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009500000724     C*
009600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009700011113     C                   Z-ADD     150           LENGH            15 5
009800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009900000724     C                   PARM                    depcmd
010000000724     C                   PARM                    LENGH
010100000724     C*
010200000724     C                   endif
010300000616     C*
010400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010500000616     C                   movel(p)  CMD(2)        depcmd
010600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010700000616     C*
010800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010900011113     C                   Z-ADD     150           LENGH            15 5
011000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011100000616     C                   PARM                    depcmd
011200000616     C                   PARM                    LENGH
011300000613     C*
011400060316     C                   if        not %open(titvs2ps)
011500060316     C                   open      titvs2ps
011600000616     C                   except    testdett
011700000613     C                   endif
011800010601     C*
011900010601     C                   endif
012000010601     C*
012100010601     C                   ENDSR
012200010601     C***
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
013300060316     C                   if        %open(titvs2ps)
013400000616     C                   except    findett
013500060316     C                   close     titvs2ps
013600000613     C                   endif
013700000616     C*
013800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013900011113     C                   Z-ADD     150           LENGH            15 5
014000010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
014100000616     C                   PARM                    CMD(3)
014200000616     C                   PARM                    LENGH
014300000616     C*
014400010601     C                   ENDSR
014500010601     C***
014600010601
014700010601
014800000613
014900000613     C*--------------------------------------------------------
015000000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015100000613     C*--------------------------------------------------------
015200000621     C     STPR          BEGSR
015300000613     C*
015400060316     C                   if        not %open(titvs2p)
015500060316     C                   open      titvs2p
015600990915     C                   endif
015700990915     C*
015800990915     C                   except    riepilogo
015900990915     C*
016000060316     C                   if        %open(titvs2p)
016100060316     C                   close     titvs2p
016200990914     C                   endif
016300000613     C*
016400000613     C                   ENDSR
016500000613     C***
016600000613
016700010601
016800010601
016900010330     C*--------------------------------------------------------
017000050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
017100010330     C*--------------------------------------------------------
017200010605     C     RWVAB         BEGSR
017300050408     C*
017400010330     C                   if        not %open(tivin00r)
017500010330     C                   open      tivin00r
017600010330     C                   endif
017700050408     C                   if        not %open(fivabwwr)
017800050408     C                   open      fivabwwr
017900010330     C                   endif
018000010330     C*
018100010604     C                   clear                   §CTROK
018200010604     C                   clear                   §CTRMO
018300010604     C                   clear                   §CTRNO
018400010330     C*
018500010330     C                   DO        *HIVAL
018600010330     C*
018700010330     C                   READ      tivin00r                               70
018800010618     C*
018900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019000010618     C                   if        vinflg = '1'
019100010618     C                   eval      flgOk = '1'
019200010618     C                   endif
019300010618     C*
019400010330     C                   if        vindta > *blanks
019500010330     C                   add       1             rrnum
019600010330     C*
019700010601     C                   if        *in70 = *off and
019800010330     C                             (vinflg = *blanks
019900010330     C                              or vinflg = '0'
020000010330     C                              or vinflg = '2')
020100010330     C*
020200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020300010711     C                   if        vinflg = *blanks or vinflg = '0'
020400010711     C                   clear                   vinmsg
020500010711     C                   endif
020600010601     C*
020700010330     C                   exsr      inzvar
020800010330     C                   exsr      defcam
020900010605     C                   exsr      impvab
021000010601     C*
021100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021200010605     C                   eval      depfil = VABLNP
021300010601     C                   exsr      repfil
021400010601     C                   if        depfil = invfil
021500021025     C                   if        vlrpoi = 999
021600021025     C                   MOVE(P)   invfil        VABFGS
021700021025     C                   else
021800021025     C                   MOVE(P)   vlrpoi        VABFGS
021900021025     C                   endif
022000010601     C*
022100010601     C                   exsr      PREELA
022200010601     C*
022300010604     C* Ebbene...
022400010604     C*
022500010604     C  N31              ADD       1             §CTROK            7 0
022600010604     C   32              ADD       1             §CTRMO            7 0
022700010604     C   31              ADD       1             §CTRNO            7 0
022800020205     C*
022900050408     C* Scarico il buffer del FIVAB
023000050408     C  N31              WRITE     FIVAB000
023100010604     C*
023200020722     C                   endif
023300020722     C*
023400010604     C                   if        *in31 = *off and
023500010604     C                             *in32 = *off
023600010604     C                   eval      vinflg = '1'
023700010604     C                   else
023800010604     C                   eval      recko = vindta
023900010604     C                   except    rigadett
024000010604     C                   eval      vinflg = '2'
024100010604     C                   endif
024200010604     C                   endif
024300010604     C*
024400010330     C                   else
024500010330     C                   eval      vinflg = '1'
024600010330     C                   endif
024700010601     C*
024800010601     C  N70              update    tivin000
024900010330     C*
025000010330     C  N70              ENDdo
025100010601     C*
025200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025300010601     C                   if        cntNonEl = *zeros or
025400010601     C                             flgMulti = '0'
025500010330     C* Se non ci sono record con errori ...
025600010601     C                   if        §ctrno = 0 and
025700010604     C                             §ctrmo = 0 and
025800010601     C                             flgStato <> '2'
025900010330     C* ... restituisco esito OK.
026000010330     C                   eval      wrkesito = '0'
026100010330     C                   else
026200010330     C                   if        §ctrok > 0
026300010330     C                   eval      wrkesito = '1'
026400010330     C                   else
026500010615     C                   if        flgOk = '0'
026600010615     C                   eval      wrkesito = '2'
026700010615     C                   else
026800010615     C                   eval      wrkesito = '6'
026900010615     C                   endif
027000010330     C                   endif
027100010330     C                   endif
027200010601     C                   else
027300010601     C                   eval      wrkesito = '9'
027400010601     C                   endif
027500010330     C*
027600010330     C                   if        %open(tivin00r)
027700010330     C                   close     tivin00r
027800010330     C                   endif
027900050408     C                   if        %open(fivabwwr)
028000050408     C                   close     fivabwwr
028100010330     C                   endif
028200010601     C*
028300010601     C                   if        vlrpoi <> 999
028400010601     C                   eval      invfil = vlrpoi
028500010601     C                   endif
028600010330     C*
028700010330     C                   if        §ctrok > 0
028800010601     C                             and invfil > *zeros
028900010330     C                   exsr      invio
029000010330     C                   endif
029100010601     C*
029200010618     C                   if        flgGiro = '1'
029300010601     C                   exsr      endela
029400010618     C                   endif
029500010330     C*
029600010330     C                   ENDSR
029700010330     C***
029800010330
029900010601
030000010601
030100010601
030200010601
030300010330     C*----------------------------------------------------*
030400020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030500010330     C*----------------------------------------------------*
030600010330     C     INZVAR        BEGSR
030700010330     C*
030800020204     C                   Z-ADD     *zeros        Num5_0            5 0
030900020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031000020322     C                   MOVEL     '0'           FlgCAS            1
031100010330     C*
031200010330     C                   ENDSR
031300010330     C*----------------------------------------------------*
031400020722     C*  IMPOSTAZIONE CAMPI COSTANTI
031500010330     C*----------------------------------------------------*
031600010330     C     DEFCAM        BEGSR
031700010330     C*
031800020204     C* Inizializzo il buffer del record da scrivere
031900050408     C                   CLEAR                   FIVAB000
032000020204     C* Imposto i valori di default...
032100060316     C                   EVAL      VABCCM = 0062232
032200060316     C                   EVAL      VABLNP = 006
032300060105     C                   EVAL      VABCTR = 000
032400060303     C                   EVAL      VABCBO = '1'
032500020204     C* ... e poi verifico se sono stati passati come parametri
032600020204     C                   IF        vlrppt > *blanks
032700040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
032800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032900020204     C                   EXSR      CHKNUM
033000020204     C                   IF        PiInt=*on
033100020204     C                   Z-ADD     PiVal         VABCCM
033200020204     C                   ENDIF
033300040301     C                   ENDIF
033400040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
033500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033600020204     C                   EXSR      CHKNUM
033700020204     C                   IF        PiInt=*on
033800020204     C                   Z-ADD     PiVal         VABLNP
033900020204     C                   ENDIF
034000040301     C                   ENDIF
034100040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
034200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034300020204     C                   EXSR      CHKNUM
034400020204     C                   IF        PiInt=*on
034500020204     C                   Z-ADD     PiVal         VABCTR
034600040301     C                   ENDIF
034700020204     C                   ENDIF
034800020204     C                   ENDIF
034900020204     C*
035000010330     C                   ENDSR
035100010607     C*----------------------------------------------------*
035200050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
035300010607     C*----------------------------------------------------*
035400010607     C     IMPVAB        BEGSR
035500010607     C*
035600010607     C                   SETOFF                                       3132
035700010607     C*
035800010607     C* Reperimento campi ALFA
035900010607     C*
036000010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
036100060316     C                   EVAL      VABRSD=%trim(%subst(vindta:112:32))
036200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
036300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
036400020117     C* ==
036500060316     C                   EVAL      VABIND=%trim(%subst(vindta:144:32))
036600060316     C                   EVAL      VABLOD=%trim(%subst(vindta:181:27))
036700060316     C                   EVAL      VABRMA=%trim(%subst(vindta:335:14))
036800060316     C                   EVAL      VABNOT=%trim(%subst(vindta:242:38))
036900060316     C                   EVAL      VABCBO=%trim(%subst(vindta:225:1))
037000100525     C                   EVAL      VABNZD=%trim(%subst(vindta:405:3))
037100100525     C*
037200100525     C* ...passo dal codice ISO a quello "Bartolini"
037300100525     C                   Z-ADD     1             jNAZ
037400100525     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
037500100525     C                   IF        %found
037600100525     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
037700100525     C                   ENDIF
037800100525     C                   IF        VABNZD = 'IT'  OR
037900100525     C                             VABNZD = 'ITA'
038000100525     C                   EVAL      VABNZD = *blanks
038100100525     C                   ENDIF
038200010607     C*
038300010607     C* Reperimento campi NUMERICI
038400020722     C                   MOVEL     DATCOR        VABAAS
038500020722     C                   MOVE      DATCOR        VABMGS
038600060105     C* RMN/NSP
038700060316     C                   EVAL      PiStr=%trim(%subst(vindta:1:6))
038800050408     C                   EXSR      CHKNUM
038900050408     C                   IF        PiInt=*on
039000050408     C                   Z-ADD     PiVal         VABRMN
039100050908     C                   Z-ADD     PiVal         VABNSP
039200050408     C                   ELSE
039300051219     C                   SETON                                        31
039400050408     C                   Z-ADD     *zeros        VABRMN
039500050408     C                   EVAL      vinmsg = %trimr(vinmsg)
039600050908     C                             + ' ' + 'VABRMN VABNSP'
039700050408     C                   ENDIF
039800040420     C* CAD
039900060316     C                   EVAL      PiStr=%trim(%subst(vindta:176:5))
040000010607     C                   EXSR      CHKNUM
040100010607     C                   IF        PiInt=*on
040200020204     C                   Z-ADD     PiVal         Num5_0
040300020204     C                   MOVEL(p)  Num5_0        VABCAD
040400010607     C                   ELSE
040500010607     C                   SETON                                        32
040600020204     C                   EVAL      VABCAD = *zeros
040700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040800020204     C                             + ' ' + 'VABCAD'
040900010607     C                   ENDIF
041000060316     C* Reperisco la provincia dal CAP e dalla località
041100100224     C                   IF        VABNZD  = *blanks AND
041200100224     C                             VABCAD <> *blanks AND
041300060316     C                             VABPRD  = *blanks
041400060316     C                   CLEAR                   TISI95DS
041500060316     C                   EVAL      I95TCN = '3'
041600060316     C                   Z-ADD     datcor        I95DAT
041700060316     C                   EVAL      I95CAP = VABCAD
041800060316     C                   EVAL      I95LOC = VABLOD
041900060316     C                   EVAL      I95NAR = VABNZD
042000060316     C                   CALL      'TISI95R'
042100060316     C                   PARM                    TISI95DS
042200060316     C                   EVAL      VABPRD = O95PRV
042300060316     C                   ENDIF
042400040420     C* NCL
042500060316     C                   EVAL      PiStr=%trim(%subst(vindta:208:4))
042600010607     C                   EXSR      CHKNUM
042700010607     C                   IF        PiInt=*on
042800010607     C                   Z-ADD     PiVal         VABNCL
042900010607     C                   ELSE
043000010607     C                   SETON                                        32
043100010607     C                   Z-ADD     *zeros        VABNCL
043200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043300010607     C                             + ' ' + 'VABNCL'
043400010607     C                   ENDIF
043500040420     C* PKB
043600060316     C                   EVAL      PiStr=%trim(%subst(vindta:213:5))
043700010607     C                   EXSR      CHKNUM
043800010607     C                   IF        PiNum=*on
043900060316     C                   Z-ADD     PiVal         VABPKB
044000010607     C                   ELSE
044100010607     C                   SETON                                        32
044200010607     C                   Z-ADD     *zeros        VABPKB
044300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
044400010607     C                             + ' ' + 'VABPKB'
044500010607     C                   ENDIF
044600070521     C* CAS
044700070521     C                   IF        %subst(vindta:227:7) <> *all'0'
044800070521     C                   EVAL      FlgCAS='1'
044900070521     C                   EVAL      VABVCA='EUR'
045000070521     C                   EVAL      PiStr=%trim(%subst(vindta:227:7))
045100070521     C                   EXSR      CHKNUM
045200070521     C                   IF        PiNum=*on
045300070521     C                   EVAL(H)   PiVal=PiVal/100
045400070521     C                   Z-ADD     PiVal         VABCAS
045500070530     C***                EVAL      VABVCA=%trim(%subst(vindta:277:3))
045600070521     C                   ELSE
045700070521     C                   SETON                                        32
045800070521     C                   EVAL      vinmsg = %trimr(vinmsg)
045900070521     C                             + ' ' + 'VABCAS'
046000070521     C                   ENDIF
046100070521     C                   ENDIF
046200020322     C*
046300020322     C* Considerazioni finali su CBO/CAS
046400050908     C                   IF        FlgCAS = '1'
046500040713     C                   IF        VABCBO = '1'
046600020322     C                   EVAL      VABCBO = '4'
046700040713     C                   ENDIF
046800040713     C                   IF        VABCBO = '2'
046900040713     C                   EVAL      VABCBO = '6'
047000040713     C                   ENDIF
047100020322     C                   ENDIF
047200020204     C*
047300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
047400020204     C                   EXSR      CHKIMPDIV
047500020204     C*
047600010607     C                   ENDSR
047700010607     C*----------------------------------------------------*
047800010601
047900020204
048000020204     C*----------------------------------------------------*
048100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048200020204     C*----------------------------------------------------*
048300020204     C     CHKIMPDIV     BEGSR
048400020204     C*
048500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
048600020204     C                   Z-ADD     *zeros        wrkDec            9 9
048700020204     C*
048800020204     C* Come prima cosa effettuo considerazioni sulla divisa
048900020204     C                   IF        vabIAS > *zeros
049000020204     C                   IF        vabVAS <> 'EUR'
049100020204     C                   EVAL      vabVAS =  'ITL'
049200020204     C                   ENDIF
049300020204     C                   ENDIF
049400020204     C*
049500020204     C                   IF        vabCAS > *zeros
049600020204     C                   IF        vabVCA <> 'EUR'
049700020204     C                   EVAL      vabVCA =  'ITL'
049800020204     C                   ENDIF
049900020204     C                   ENDIF
050000020204     C*
050100020204     C                   IF        vabVMD > *zeros
050200020321     C                   IF        vabVAD <> 'EUR'
050300020204     C                   EVAL      vabVAD =  'ITL'
050400020204     C                   ENDIF
050500020204     C                   ENDIF
050600020204     C*
050700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050800020204     C                   Z-ADD     vabIAS        wrkDec
050900020204     C                   IF        wrkDec > *zeros
051000020204     C                   IF        vabVAS = 'ITL'
051100020204     C                   EVAL      vabIAS = *zeros
051200020204     C                   ENDIF
051300020204     C                   ENDIF
051400020204     C*
051500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
051600020204     C                   Z-ADD     vabCAS        wrkDec
051700020204     C                   IF        wrkDec > *zeros
051800020204     C                   IF        vabVCA = 'ITL'
051900020204     C                   EVAL      vabCAS = *zeros
052000020204     C                   ENDIF
052100020204     C                   ENDIF
052200020204     C*
052300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052400020204     C                   Z-ADD     vabVMD        wrkDec
052500020204     C                   IF        wrkDec > *zeros
052600020204     C                   IF        vabVAD = 'ITL'
052700020204     C                   EVAL      vabVMD = *zeros
052800020204     C                   ENDIF
052900020204     C                   ENDIF
053000020204     C*
053100020204     C                   ENDSR
053200020204     C***
053300020204
053400010330
053500010330
053600010330     C*----------------------------------------------------*
053700010330     C*  CONTROLLO NUMERICITA' CAMPI
053800010330     C*----------------------------------------------------*
053900010330     C     CHKNUM        BEGSR
054000010330     C*
054100010606     C                   IF        PiDecChr = *blanks
054200010606     C                   EVAL      PiDecChr = ','
054300010606     C                   ENDIF
054400010606     C*
054500010606     C                   CALL(e)   'ISNUMERIC'
054600010330     C                   PARM                    PiStr            30
054700010606     C                   PARM                    PiDecChr          1
054800010330     C                   PARM      *ZEROS        PiVal            30 9
054900010330     C                   PARM      '0'           PiInt             1
055000010330     C                   PARM      '0'           PiNum             1
055100010330     C                   IF        %error
055200010606     C                   EVAL      PiNum=*off
055300010330     C                   ENDIF
055400010330     C*
055500010330     C                   ENDSR
055600010330     C***
055700010330
055800010601
055900010601
056000010601
056100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
056200010601     C     repfil        BEGSR
056300010601     C*
056400010601     C                   if        invfil = *zeros and
056500010601     C                             depfil > *zeros and
056600010629     C                             (vinflg = *blanks or
056700010629     C                              vinflg = *zeros)
056800010601     C
056900010601     C                   eval      invfil = depfil
057000010601     C                   endif
057100010601     C*
057200010601     C                   if        depfil <> invfil and
057300010601     C                             invfil > *zeros
057400010601     C                   eval      flgMulti = '1'
057500010601     C                   if        vinflg = *blanks
057600010601     C                   add       1             cntNonEl
057700010601     C                   endif
057800010601     C                   endif
057900010601     C*
058000010601     C                   if        vinflg = '2'
058100010601     C                   eval      flgStato = '2'
058200010601     C                   endif
058300010601     C*
058400010601     C                   ENDSR
058500010601     C***
058600010601
058700010601
058800010601
058900010330
059000010330
059100010330
059200990920      /TITLE Invio dei dati al punto operativo.
059300000613     C     invio         BEGSR
059400990920     C*
059500990920     C                   reset                   dscmz
059600010601     C                   move      invfil        cmzdst
059700990920     C                   eval      cmzfld = vlrfou
059800990920     C                   eval      cmzmbd = vlrhdl
059900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
060000000710     C                   if        prmfir = *blanks
060100050408     C                   eval      cmzfla = 'FIVAB00F'
060200050408     C                   eval      cmzmba = 'FIVAB00F'
060300000710     C                   else
060400000710     C                   eval      cmzfla = prmfir
060500000710     C                   eval      cmzmba = prmfir
060600000710     C                   endif
060700990920     C                   eval      cmznrr = *zeros
060800990920     C                   move      §ctrok        cmznrr
060900021018     C                   eval      cmzlba = vlrfl1
061000990920     C                   call(e)   'TIS711C'
061100990920     C                   parm                    dscmz
061200990921     C                   parm      *blanks       esito
061300990920     C                   if        %error
061400990920     C                             or cmzerr = '1'
061500990921     C                             or esito  = '1'
061600000710     C                   eval      wrkesito = '3'
061700990920     C                   endif
061800990920     C*
061900000613     C                   ENDSR
062000990910
062100010601
062200010601
062300010601
062400010601
062500010601      /TITLE Invio dei dati al punto operativo.
062600010601     C     opeini        BEGSR
062700010601     C*
062800010601     C* Inizializzo flag e contatori operativi
062900010601     C                   movel     '0'           flgGiro           1
063000010601     C                   movel     '0'           flgMulti          1
063100010601     C                   movel     '1'           flgStato          1
063200010615     C                   movel     '0'           flgOk             1
063300010601     C                   z-add     *zeros        cntNonEl         10 0
063400010601     C                   z-add     *zeros        depfil            3 0
063500010601     C                   z-add     *zeros        invfil            3 0
063600010601     C*
063700010601     C                   ENDSR
063800010601     C***
063900010601
064000100525
064100100525     C*--------------------------------------------------------
064200100525     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
064300100525     C*--------------------------------------------------------
064400100525     C     CARTAB        BEGSR
064500100525     C*
064600100525     C* TABELLA '15' - NAZIONI
064700100525     C                   clear                   skNAZISO
064800100525     C                   clear                   skNAZBAR
064900100525     C                   eval      tblKUT = 1
065000100525     C                   eval      tblCOD = '15'
065100100525     C     KEYtabP       setll     tabel00f
065200100525     C     KEYtabP       reade     tabel00f
065300100525     C                   dow       not %eof(tabel00f)
065400100525     C                   if        tblFLG = *blanks
065500100525     C                   movel(p)  tblUNI        ds15
065600100525     C                   add       1             jNAZ
065700100525     C                   eval      skNAZBAR(jNAZ) = tblKEY
065800100525     C                   eval      skNAZISO(jNAZ) = §15COD
065900100525     C                   endif
066000100525     C     KEYtabP       reade     tabel00f
066100100525     C                   enddo
066200100525     C*
066300100525     C                   ENDSR
066400100525     C***
066500010601
066600010330
066700010330
066800000613     C     *inzsr        BEGSR
066900990910     C*
067000990910     C     *entry        plist
067100990920     C                   parm                    tivlrds
067200990921     C                   parm      wrkesito      esito
067300000724     C                   parm                    prmlit
067400000710     C                   parm                    prmfir
067500100525     C*
067600100525     C* Chiave su TABEL00F - parziale
067700100525     C     KEYtabP       KLIST
067800100525     C                   KFLD                    tblKUT
067900100525     C                   KFLD                    tblCOD
068000010330     C*
068100010330     C* CALCOLA LA DATA CORRENTE
068200010330     C                   time                    wn14             14 0
068300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068400010330     C                   z-add     wn8           g08dat
068500010330     C                   z-add     *zeros        g08inv
068600010330     C                   movel     '0'           g08err
068700010330     C                   call      'XSRDA8'
068800010330     C                   parm                    wlbda8
068900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069000000613     C*
069100000613     C                   ENDSR
069200000613     C***
069300990908
069400060316     Otitvs2p   E            riepilogo         2
069500990915     O                                              'Upload via Internet'
069600990915     O                                           +1 'Traduzione TIVIN00R -'
069700050408     O                                           +1 'FIVABWWR'
069800990915     O                                           +1 'Testate bolle'
069900990915     O          E            riepilogo   2
070000990915     O                       wrkdata
070100990915     O                       wrkora              +1
070200990915     O                       procname            +1
070300990915     O          E            riepilogo   2
070400990915     O                                              'Cliente..................:'
070500010605     O                       VABCCM        z     +1
070600990915     O          E            riepilogo   2
070700990920     O                                              'Riferimento Strategi.....:'
070800990920     O                       vlrhdl              +1
070900990915     O          E            riepilogo   2
071000990915     O                                              'Giusti...................:'
071100971022     O                       §CTROK        2   +  1
071200990915     O          E            riepilogo   2
071300990915     O                                              'Sbagliati e corretti.....:'
071400971022     O                       §CTRMO        2   +  1
071500990915     O          E            riepilogo   2
071600990915     O                                              'Sbagliati e scartati.....:'
071700971022     O                       §CTRNO        2   +  1
071800000613
071900060316     Otitvs2ps  E            testdett          2
072000000613     O                                              'Upload via Internet'
072100000613     O                                           +1 'Traduzione TIVIN00R -'
072200050408     O                                           +1 'FIVABWWR'
072300000613     O                                           +1 'Testate bolle'
072400000616     O          E            testdett    3
072500000613     O                                           +2 'N° rec'
072600000613     O                                           +3 'Anteprima contenuto'
072700000616     O          E            rigadett    2
072800000613     O                       rrnum               +2
072900000621     O                       recko               +3
073000000616     O          E            findett     2
073100000613     O                       wrkdata
073200000613     O                       wrkora              +1
073300000613     O                       procname            +1
073400000616     O          E            findett     2
073500000613     O                                              'Cliente..................:'
073600010605     O                       VABCCM        z     +1
073700000616     O          E            findett     2
073800000613     O                                              'Riferimento Strategi.....:'
073900000613     O                       vlrhdl              +1
074000000616     O          E            findett     2
074100000613     O                                              'Giusti...................:'
074200000613     O                       §CTROK        2   +  1
074300000616     O          E            findett     2
074400000613     O                                              'Sbagliati e corretti.....:'
074500000613     O                       §CTRMO        2   +  1
074600000616     O          E            findett     2
074700000613     O                                              'Sbagliati e scartati.....:'
074800000613     O                       §CTRNO        2   +  1
074900000613** CMD - COMANDI CL
075000060316OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVS2P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
075100060316OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVS2PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
075200060316DLTOVR FILE(TITVS2P TITVS2PS) LVL(*)
075300010606
075400000613
