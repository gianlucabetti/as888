000100040317      /TITLE Upload via Internet: traduzione in FIVABWWR. (BESANA S.p.A. - Cod. 2021602/2021603)
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600020913     FFIVABwwr  O    E             DISK    usropn
000700040317     Ftitva7p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900040317     Ftitva7ps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100990908
001200000512     D*------------
001300000512     D* COMANDI
001400000512     D*------------
001500011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600000512     D*------------
001700990920     D dscmz         e ds                  inz
001800990910     D psds           sds
001900990910     D  procname         *PROC
002000990920     D tivlrds       e ds                  extname(tivlr00f)
002100990910     D esito           s              1
002200000724     D prmlit          s             10
002300000710     D prmfir          s             10
002400990921     D wrkesito        s                   like(esito)
002500990915     D wrkdata         s               d
002600990915     D wrkora          s               t
002700000613     D rrnum           s              6  0 INZ(*zeros)
002800000621     D recko           s            150    INZ(*blanks)
002900011123     D depcmd          s            150    INZ(*blanks)
003000010606     D*------------------
003100010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
003200010606     D*------------------
003300010606     D WLBDA8          DS                  INZ
003400010606     D  G08DAT                 1      8  0
003500010606     D  G08INV                 9     16  0
003600010606     D  G08ERR                17     17
003700010606     D  G08TGI                18     22  0
003800010606     D*
003900990908
004000990915     C                   time                    wrkdata
004100990915     C                   time                    wrkora
004200000913     C                   reset                   rrnum
004300990921     C                   reset                   esito
004400990921     C                   reset                   wrkesito
004500010530     C*
004600010530     C                   exsr      opeini
004700010530     C                   exsr      rwvab
004800010530     C*
004900010530     C                   seton                                        lr
005000010530
005100010530
005200010530
005300010530
005400010530     C*--------------------------------------------------------
005500010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005600010530     C*--------------------------------------------------------
005700010530     C     PREELA        BEGSR
005800000724     C*
005900000724     C* SE OCCORRE SPEDIRE IN FILIALE
006000010530     C                   if        invfil <> *zeros and
006100010530     C                             flgGiro = '0'
006200010530     C*
006300010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006400010530     C                   eval      flgGiro = '1'
006500000724     C*
006600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006800010530     C     invfil        chain     azorg01l
006900000724     C                   if        %found
007000000616     C                   movel(p)  CMD(1)        depcmd
007100020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007200000616     C*
007300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007400011123     C                   Z-ADD     150           LENGH            15 5
007500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007600000616     C                   PARM                    depcmd
007700000616     C                   PARM                    LENGH
007800000724     C*
007900000724     C                   endif
008000000616     C*
008100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008200000616     C                   movel(p)  CMD(2)        depcmd
008300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008400000616     C*
008500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008600011123     C                   Z-ADD     150           LENGH            15 5
008700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008800000616     C                   PARM                    depcmd
008900000616     C                   PARM                    LENGH
009000000616     C*
009100040317     C                   if        not %open(titva7ps)
009200040317     C                   open      titva7ps
009300030407     C   50              except    testdett
009400000613     C                   endif
009500010530     C*
009600010530     C                   endif
009700010530     C*
009800010530     C                   ENDSR
009900010530     C***
010000010530
010100010530
010200010530
010300010530
010400010530
010500010530
010600010530     C*--------------------------------------------------------
010700010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010800010530     C*--------------------------------------------------------
010900010530     C     ENDELA        BEGSR
011000010530     C*
011100010530     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011200000613     C*
011300040317     C                   if        %open(titva7ps)
011400030407     C   50              except    findett
011500040317     C                   close     titva7ps
011600000613     C                   endif
011700000616     C*
011800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011900011123     C                   Z-ADD     150           LENGH            15 5
012000010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012100000616     C                   PARM                    CMD(3)
012200000616     C                   PARM                    LENGH
012300010530     C*
012400010530     C                   ENDSR
012500010530     C***
012600000613
012700010530
012800010530
012900000613
013000000613     C*--------------------------------------------------------
013100000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013200000613     C*--------------------------------------------------------
013300000621     C     STPR          BEGSR
013400000613     C*
013500040317     C                   if        not %open(titva7p)
013600040317     C                   open      titva7p
013700990915     C                   endif
013800990915     C*
013900030407     C   50              except    riepilogo
014000990915     C*
014100040317     C                   if        %open(titva7p)
014200040317     C                   close     titva7p
014300990914     C                   endif
014400990910     C*
014500000613     C                   ENDSR
014600000613     C***
014700990908
014800010530
014900010530
015000010530
015100910830     C*--------------------------------------------------------
015200020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
015300910830     C*--------------------------------------------------------
015400941229     C     RWVAB         BEGSR
015500990910     C*
015600990914     C                   if        not %open(tivin00r)
015700990908     C                   open      tivin00r
015800990914     C                   endif
015900020913     C                   if        not %open(fivabwwr)
016000020913     C                   open      fivabwwr
016100990914     C                   endif
016200990910     C*
016300990910     C                   clear                   §CTROK
016400990910     C                   clear                   §CTRMO
016500990910     C                   clear                   §CTRNO
016600990910     C*
016700921023     C                   DO        *HIVAL
016800990913     C*
016900990915     C                   READ      tivin00r                               70
017000010618     C*
017100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017200010618     C                   if        vinflg = '1'
017300010618     C                   eval      flgOk = '1'
017400010618     C                   endif
017500010705     C*
017600010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
017700010705     C                   if        %subst(vindta:1:1) = '='
017800010705     C                   eval      %subst(vindta:1:1) = *blanks
017900010705     C                   endif
018000010705     C*
018100000905     C                   if        vindta > *blanks
018200000613     C                   add       1             rrnum
018300010601     C* ...elimino eventuali segni
018400010601     C     '+':' '       xlate     vindta        vindta
018500990913     C*
018600010601     C                   if        *in70 = *off and
018700990913     C                             (vinflg = *blanks
018800010711     C                              or vinflg = '0'
018900010711     C                              or vinflg = '2')
019000990913     C*
019100020913     C                   clear                   FIVAB000
019200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019300010711     C                   if        vinflg = *blanks or vinflg = '0'
019400010711     C                   clear                   vinmsg
019500010711     C                   endif
019600040316     C*
019700040316     C                   exsr      inzvar
019800040316     C                   exsr      defcam
019900040316     C                   exsr      impvab
020000010530     C*
020100010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020200010530     C                   eval      depfil = VABLNP
020300010530     C                   exsr      repfil
020400010601     C                   if        depfil = invfil
020500021014     C                   if        vlrpoi = 999
020600021010     C                   MOVE(P)   invfil        VABFGS
020700021014     C                   else
020800021014     C                   MOVE(P)   vlrpoi        VABFGS
020900021014     C                   endif
021000010530     C*
021100010530     C                   exsr      PREELA
021200930409     C*
021300010604     C  N31              ADD       1             §CTROK            7 0
021400010604     C   32              ADD       1             §CTRMO            7 0
021500010604     C   31              ADD       1             §CTRNO            7 0
021600020913     C  N31              WRITE     FIVAB000
021700990910     C*
021800010601     C                   if        *in31 = *off and
021900010601     C                             *in32 = *off
022000990910     C                   eval      vinflg = '1'
022100990910     C                   else
022200000621     C                   eval      recko = vindta
022300040317     C                   if        %open(titva7ps)
022400030407     C   50              except    rigadett
022500010612     C                   endif
022600990910     C                   eval      vinflg = '2'
022700990910     C                   endif
022800991022     C                   endif
022900010601     C*
023000010601     C                   endif
023100000905     C*
023200000905     C                   else
023300000905     C                   eval      vinflg = '1'
023400000905     C                   endif
023500000905     C*
023600000905     C  N70              update    tivin000
023700991022     C*
023800991022     C  N70              ENDdo
023900010530     C
024000010530     C*
024100010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024200010530     C                   if        cntNonEl = *zeros or
024300010530     C                             flgMulti = '0'
024400990910     C* Se non ci sono record con errori ...
024500010530     C                   if        §ctrno = 0 and
024600010601     C                             §ctrmo = 0 and
024700010530     C                             flgStato <> '2'
024800990910     C* ... restituisco esito OK.
024900990921     C                   eval      wrkesito = '0'
025000990910     C                   else
025100010601     C                   if        §ctrok > 0
025200990921     C                   eval      wrkesito = '1'
025300000710     C                   else
025400010615     C                   if        flgOk = '0'
025500010615     C                   eval      wrkesito = '2'
025600010615     C                   else
025700010615     C                   eval      wrkesito = '6'
025800010615     C                   endif
025900990910     C                   endif
026000000710     C                   endif
026100010530     C                   else
026200010530     C                   eval      wrkesito = '9'
026300010530     C                   endif
026400990910     C*
026500990914     C                   if        %open(tivin00r)
026600990908     C                   close     tivin00r
026700990914     C                   endif
026800020913     C                   if        %open(fivabwwr)
026900020913     C                   close     fivabwwr
027000990914     C                   endif
027100010530     C*
027200010530     C                   if        vlrpoi <> 999
027300010530     C                   eval      invfil = vlrpoi
027400010530     C                   endif
027500990910     C*
027600990920     C                   if        §ctrok > 0
027700010530     C                             and invfil > *zeros
027800000613     C                   exsr      invio
027900990920     C                   endif
028000010612     C*
028100010612     C                   if        flgGiro = '1'
028200010612     C                   exsr      endela
028300010612     C                   endif
028400990920     C*
028500910830     C                   ENDSR
028600000613     C***
028700040316
028800040316
028900040316
029000040316
029100040316
029200040316     C*----------------------------------------------------*
029300040316     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029400040316     C*----------------------------------------------------*
029500040316     C     INZVAR        BEGSR
029600040316     C*
029700040316     C                   Z-ADD     *zeros        Num5_0            5 0
029800040316     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029900040316     C                   MOVEL     '0'           FlgCAS            1
030000040316     C*
030100040316     C                   ENDSR
030200040316     C*----------------------------------------------------*
030300040316     C*  IMPOSTAZIONE CAMPI COSTANTI
030400040316     C*----------------------------------------------------*
030500040316     C     DEFCAM        BEGSR
030600040316     C*
030700040316     C* Inizializzo il buffer del record da scrivere
030800040316     C                   CLEAR                   FIVAB000
030900040316     C*
031000040316     C* Valorizzo i campi costanti
031100040317     C                   eval      VABCCM = 2021603
031200040317     C                   eval      VABLNP = 202
031300040325     C                   eval      VABCTR = 100
031400040317     C                   eval      VABTSP = 'C'
031500040316     C* ... e poi verifico se sono stati passati come parametri
031600040316     C                   IF        vlrppt > *blanks
031700040324     C                   IF        %trim(%subst(vlrppt:2:7)) <> *blanks
031800040324     C                   EVAL      PiStr=%trim(%subst(vlrppt:2:7))
031900040316     C                   EXSR      CHKNUM
032000040316     C                   IF        PiInt=*on
032100040316     C                   Z-ADD     PiVal         VABCCM
032200040316     C                   ENDIF
032300040316     C                   ENDIF
032400040324     C                   IF        %trim(%subst(vlrppt:9:3)) <> *blanks
032500040324     C                   EVAL      PiStr=%trim(%subst(vlrppt:9:3))
032600040316     C                   EXSR      CHKNUM
032700040316     C                   IF        PiInt=*on
032800040316     C                   Z-ADD     PiVal         VABLNP
032900040316     C                   ENDIF
033000040316     C                   ENDIF
033100040324     C                   IF        %trim(%subst(vlrppt:12:3)) <> *blanks
033200040324     C                   EVAL      PiStr=%trim(%subst(vlrppt:12:3))
033300040316     C                   EXSR      CHKNUM
033400040316     C                   IF        PiInt=*on
033500040316     C                   Z-ADD     PiVal         VABCTR
033600040316     C                   ENDIF
033700040316     C                   ENDIF
033800040316     C                   ENDIF
033900040316     C*
034000040316     C                   ENDSR
034100040316     C*----------------------------------------------------*
034200040316     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034300040316     C*----------------------------------------------------*
034400040316     C     IMPVAB        BEGSR
034500040316     C*
034600040316     C                   SETOFF                                       3132
034700040316     C*
034800040316     C* Reperimento campi ALFA
034900040316     C*
035000040316     C* Considerazioni sulla ragione sociale del destinatario da indicare
035100040317     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
035200040316     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035300040316     C     '@':'A'       XLATE     VABRSD        VABRSD
035400040316     C* ==
035500040317     C                   EVAL      VABRD2=%trim(%subst(vindta:36:35))
035600040317     C                   EVAL      VABIND=%trim(%subst(vindta:71:35))
035700040317     C                   EVAL      VABLOD=%trim(%subst(vindta:115:35))
035800040317     C                   EVAL      VABPRD=%trim(%subst(vindta:148:2))
035900051019     C                   EVAL      VABNZD=%trim(%subst(vindta:200:3))
036000050720     C                   EVAL      VABRMA=%trim(%subst(vindta:163:15))
036100040316     C*
036200040316     C* Reperimento campi NUMERICI
036300040316     C                   MOVEL     DATCOR        VABAAS
036400040316     C                   MOVE      DATCOR        VABMGS
036500040316     C* NSP/RMN
036600050720     C                   EVAL      PiStr=%trim(%subst(vindta:163:15))
036700040316     C                   EXSR      CHKNUM
036800040316     C                   IF        PiInt=*on
036900040316     C                   Z-ADD     PiVal         VABNSP
037000040316     C                   Z-ADD     PiVal         VABRMN
037100040316     C                   ELSE
037200040316     C                   SETON                                        31
037300040316     C                   Z-ADD     *zeros        VABNSP
037400040316     C                   Z-ADD     *zeros        VABRMN
037500040316     C                   EVAL      vinmsg = %trimr(vinmsg)
037600040316     C                             + ' ' + 'VABNSP VABRMN'
037700040316     C                   ENDIF
037800040316     C* CAD
037900040317     C                   EVAL      PiStr=%trim(%subst(vindta:106:5))
038000040316     C                   EXSR      CHKNUM
038100040316     C                   IF        PiInt=*on
038200040316     C                   Z-ADD     PiVal         Num5_0
038300040316     C                   MOVEL(p)  Num5_0        VABCAD
038400040316     C                   ELSE
038500040316     C                   SETON                                        32
038600040316     C                   EVAL      vinmsg = %trimr(vinmsg)
038700040316     C                             + ' ' + 'VABCAD'
038800040316     C                   ENDIF
038900040316     C* NCL
039000040317     C                   EVAL      PiStr=%trim(%subst(vindta:150:5))
039100040316     C                   EXSR      CHKNUM
039200040316     C                   IF        PiInt=*on
039300040316     C                   Z-ADD     PiVal         VABNCL
039400040316     C                   ELSE
039500040316     C                   SETON                                        32
039600040316     C                   Z-ADD     *zeros        VABNCL
039700040316     C                   EVAL      vinmsg = %trimr(vinmsg)
039800040316     C                             + ' ' + 'VABNCL'
039900040316     C                   ENDIF
040000040316     C* PKB
040100040317     C                   EVAL      PiStr=%trim(%subst(vindta:155:7))
040200040316     C                   EXSR      CHKNUM
040300040316     C                   IF        PiNum=*on
040400040317     C                   Z-ADD     PiVal         VABPKB
040500040316     C                   ELSE
040600040316     C                   SETON                                        32
040700040316     C                   Z-ADD     *zeros        VABPKB
040800040316     C                   EVAL      vinmsg = %trimr(vinmsg)
040900040316     C                             + ' ' + 'VABPKB'
041000040316     C                   ENDIF
041100040317     C* CAS
041200040317     C                   IF        %subst(vindta:188:10) <> *blanks
041300040317     C                   EVAL      FlgCAS = '1'
041400040317     C                   EVAL      PiStr=%trim(%subst(vindta:188:10))
041500040317     C                   EXSR      CHKNUM
041600040317     C                   IF        PiNum=*on
041700040317     C                   Z-ADD     PiVal         VABCAS
041800040317     C                   EVAL      VABVCA ='EUR'
041900040317     C                   ELSE
042000040317     C                   SETON                                        32
042100040317     C                   Z-ADD     *zeros        VABCAS
042200040317     C                   EVAL      vinmsg = %trimr(vinmsg)
042300040317     C                             + ' ' + 'VABCAS'
042400040317     C                   ENDIF
042500040317     C                   ENDIF
042600040317     C*
042700051019     C* Considerazioni finali su NCL/CCM (solo x NN estero)
042800051019     C                   IF        VABNZD = *blanks
042900040317     C                   IF        VABNCL > 3
043000040317     C                   EVAL      VABCCM = 2021602
043100040317     C                   ELSE
043200040317     C                   EVAL      VABCCM = 2021603
043300040317     C                   ENDIF
043400051019     C                   ENDIF
043500040316     C*
043600040316     C* Considerazioni finali su CBO/CAS
043700040316     C                   IF        FlgCAS = '1'
043800040316     C                   EVAL      VABCBO = '4'
043900040316     C                   ELSE
044000040316     C                   EVAL      VABCBO = '1'
044100040316     C                   ENDIF
044200040316     C*
044300040316     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044400040316     C                   EXSR      CHKIMPDIV
044500040316     C*
044600040316     C                   ENDSR
044700040316     C*----------------------------------------------------*
044800040316
044900040316
045000040316     C*----------------------------------------------------*
045100040316     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
045200040316     C*----------------------------------------------------*
045300040316     C     CHKIMPDIV     BEGSR
045400040316     C*
045500040316     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
045600040316     C                   Z-ADD     *zeros        wrkDec            9 9
045700040316     C*
045800040316     C* Come prima cosa effettuo considerazioni sulla divisa
045900040316     C                   IF        vabIAS > *zeros
046000040316     C                   IF        vabVAS <> 'EUR'
046100040316     C                   EVAL      vabVAS =  'ITL'
046200040316     C                   ENDIF
046300040316     C                   ENDIF
046400040316     C*
046500040316     C                   IF        vabCAS > *zeros
046600040316     C                   IF        vabVCA <> 'EUR'
046700040316     C                   EVAL      vabVCA =  'ITL'
046800040316     C                   ENDIF
046900040316     C                   ENDIF
047000040316     C*
047100040316     C                   IF        vabVMD > *zeros
047200040316     C                   IF        vabVAD <> 'EUR'
047300040316     C                   EVAL      vabVAD =  'ITL'
047400040316     C                   ENDIF
047500040316     C                   ENDIF
047600040316     C*
047700040316     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
047800040316     C                   Z-ADD     vabIAS        wrkDec
047900040316     C                   IF        wrkDec > *zeros
048000040316     C                   IF        vabVAS = 'ITL'
048100040316     C                   EVAL      vabIAS = *zeros
048200040316     C                   ENDIF
048300040316     C                   ENDIF
048400040316     C*
048500040316     C* Stabilisco se il contrasegno ha decimali valorizzati
048600040316     C                   Z-ADD     vabCAS        wrkDec
048700040316     C                   IF        wrkDec > *zeros
048800040316     C                   IF        vabVCA = 'ITL'
048900040316     C                   EVAL      vabCAS = *zeros
049000040316     C                   ENDIF
049100040316     C                   ENDIF
049200040316     C*
049300040316     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
049400040316     C                   Z-ADD     vabVMD        wrkDec
049500040316     C                   IF        wrkDec > *zeros
049600040316     C                   IF        vabVAD = 'ITL'
049700040316     C                   EVAL      vabVMD = *zeros
049800040316     C                   ENDIF
049900040316     C                   ENDIF
050000040316     C*
050100040316     C                   ENDSR
050200040316     C***
050300040316
050400040316
050500040316
050600040316     C*----------------------------------------------------*
050700040316     C*  CONTROLLO NUMERICITA' CAMPI
050800040316     C*----------------------------------------------------*
050900040316     C     CHKNUM        BEGSR
051000040316     C*
051100040316     C                   IF        PiDecChr = *blanks
051200040316     C                   EVAL      PiDecChr = ','
051300040316     C                   ENDIF
051400040316     C*
051500040316     C                   CALL(e)   'ISNUMERIC'
051600040316     C                   PARM                    PiStr            30
051700040316     C                   PARM                    PiDecChr          1
051800040316     C                   PARM      *ZEROS        PiVal            30 9
051900040316     C                   PARM      '0'           PiInt             1
052000040316     C                   PARM      '0'           PiNum             1
052100040316     C                   IF        %error
052200040316     C                   EVAL      PiNum=*off
052300040316     C                   ENDIF
052400040316     C*
052500040316     C                   ENDSR
052600040316     C***
052700990920
052800010530
052900010530
053000010530
053100010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
053200010530     C     repfil        BEGSR
053300010530     C*
053400010530     C                   if        invfil = *zeros and
053500010530     C                             depfil > *zeros and
053600010613     C                             (vinflg = *blanks or
053700010613     C                              vinflg = *zeros)
053800010530     C
053900010530     C                   eval      invfil = depfil
054000010530     C                   endif
054100010530     C*
054200010530     C                   if        depfil <> invfil and
054300010530     C                             invfil > *zeros
054400010530     C                   eval      flgMulti = '1'
054500010530     C                   if        vinflg = *blanks
054600010530     C                   add       1             cntNonEl
054700010530     C                   endif
054800010530     C                   endif
054900010530     C*
055000010530     C                   if        vinflg = '2'
055100010530     C                   eval      flgStato = '2'
055200010530     C                   endif
055300010530     C*
055400010530     C                   ENDSR
055500010530     C***
055600010530
055700010530
055800010530
055900010530
056000990920      /TITLE Invio dei dati al punto operativo.
056100000613     C     invio         BEGSR
056200990920     C*
056300990920     C                   reset                   dscmz
056400010530     C                   move      invfil        cmzdst
056500990920     C                   eval      cmzfld = vlrfou
056600990920     C                   eval      cmzmbd = vlrhdl
056700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
056800000710     C                   if        prmfir = *blanks
056900020913     C                   eval      cmzfla = 'FIVAB00F'
057000020913     C                   eval      cmzmba = 'FIVAB00F'
057100000710     C                   else
057200000710     C                   eval      cmzfla = prmfir
057300000710     C                   eval      cmzmba = prmfir
057400000710     C                   endif
057500990920     C                   eval      cmznrr = *zeros
057600990920     C                   move      §ctrok        cmznrr
057700021003     C                   eval      cmzlba = vlrfl1
057800990920     C                   call(e)   'TIS711C'
057900990920     C                   parm                    dscmz
058000990921     C                   parm      *blanks       esito
058100990920     C                   if        %error
058200990920     C                             or cmzerr = '1'
058300990921     C                             or esito  = '1'
058400000710     C                   eval      wrkesito = '3'
058500990920     C                   endif
058600990920     C*
058700000613     C                   ENDSR
058800000613     C***
058900010530
059000010530
059100010530
059200010530
059300010530      /TITLE Invio dei dati al punto operativo.
059400010530     C     opeini        BEGSR
059500010530     C*
059600010530     C* Inizializzo flag e contatori operativi
059700010530     C                   movel     '0'           flgGiro           1
059800010530     C                   movel     '0'           flgMulti          1
059900010530     C                   movel     '1'           flgStato          1
060000010615     C                   movel     '0'           flgOk             1
060100010530     C                   z-add     *zeros        cntNonEl         10 0
060200010530     C                   z-add     *zeros        depfil            3 0
060300010530     C                   z-add     *zeros        invfil            3 0
060400030407     C*
060500030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
060600030407     C                   if        %subst(vlrppt:1:1) = 'S'
060700030407     C                   seton                                        50
060800030407     C                   else
060900030407     C                   setoff                                       50
061000030407     C                   endif
061100010530     C*
061200010530     C                   ENDSR
061300010530     C***
061400010530
061500010530
061600010530
061700990910
061800000613     C     *inzsr        BEGSR
061900990910     C*
062000990910     C     *entry        plist
062100990920     C                   parm                    tivlrds
062200990921     C                   parm      wrkesito      esito
062300000724     C                   parm                    prmlit
062400000710     C                   parm                    prmfir
062500010606     C*
062600010606     C* CALCOLA LA DATA CORRENTE
062700010606     C                   time                    wn14             14 0
062800010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062900010606     C                   z-add     wn8           g08dat
063000010606     C                   z-add     *zeros        g08inv
063100010606     C                   movel     '0'           g08err
063200010606     C                   call      'XSRDA8'
063300010606     C                   parm                    wlbda8
063400010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063500000613     C*
063600000613     C                   ENDSR
063700000613     C***
063800990908
063900040317     Otitva7p   E            riepilogo         2
064000990915     O                                              'Upload via Internet'
064100990915     O                                           +1 'Traduzione TIVIN00R -'
064200020913     O                                           +1 'FIVABWWR'
064300990915     O                                           +1 'Testate bolle'
064400990915     O          E            riepilogo   2
064500990915     O                       wrkdata
064600990915     O                       wrkora              +1
064700990915     O                       procname            +1
064800990915     O          E            riepilogo   2
064900990915     O                                              'Cliente..................:'
065000990915     O                       VABCCM        z     +1
065100990915     O          E            riepilogo   2
065200990920     O                                              'Riferimento Strategi.....:'
065300990920     O                       vlrhdl              +1
065400990915     O          E            riepilogo   2
065500990915     O                                              'Giusti...................:'
065600971022     O                       §CTROK        2   +  1
065700990915     O          E            riepilogo   2
065800990915     O                                              'Sbagliati e corretti.....:'
065900971022     O                       §CTRMO        2   +  1
066000990915     O          E            riepilogo   2
066100990915     O                                              'Sbagliati e scartati.....:'
066200971022     O                       §CTRNO        2   +  1
066300000613
066400040317     Otitva7ps  E            testdett          2
066500000613     O                                              'Upload via Internet'
066600000613     O                                           +1 'Traduzione TIVIN00R -'
066700020913     O                                           +1 'FIVABWWR'
066800000613     O                                           +1 'Testate bolle'
066900000616     O          E            testdett    3
067000000613     O                                           +2 'N° rec'
067100000613     O                                           +3 'Anteprima contenuto'
067200000616     O          E            rigadett    2
067300000613     O                       rrnum               +2
067400000621     O                       recko               +3
067500000616     O          E            findett     2
067600000613     O                       wrkdata
067700000613     O                       wrkora              +1
067800000613     O                       procname            +1
067900000616     O          E            findett     2
068000000613     O                                              'Cliente..................:'
068100000613     O                       VABCCM        z     +1
068200000616     O          E            findett     2
068300000613     O                                              'Riferimento Strategi.....:'
068400000613     O                       vlrhdl              +1
068500000616     O          E            findett     2
068600000613     O                                              'Giusti...................:'
068700000613     O                       §CTROK        2   +  1
068800000616     O          E            findett     2
068900000613     O                                              'Sbagliati e corretti.....:'
069000000613     O                       §CTRMO        2   +  1
069100000616     O          E            findett     2
069200000613     O                                              'Sbagliati e scartati.....:'
069300000613     O                       §CTRNO        2   +  1
069400000512** CMD - COMANDI CL
069500040317OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVA7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
069600040317OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVA7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
069700040317DLTOVR FILE(TITVA7P TITVA7PS) LVL(*)
069800000512
069900000512
