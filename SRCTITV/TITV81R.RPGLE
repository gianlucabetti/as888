000100031006      /TITLE Upload via Internet: traduzione in FIVABWWR. (x OCULAR SCIENCES - Cod. 0052561)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700031215     Ftitv81p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900031215     Ftitv81ps  O    f  198        PRINTER usropn
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
002300031215     D tisi95ds      e ds
002400010330     D esito           s              1
002500010330     D prmlit          s             10
002600010330     D prmfir          s             10
002700010330     D wrkesito        s                   like(esito)
002800010330     D wrkdata         s               d
002900010330     D wrkora          s               t
003000010330     D rrnum           s              6  0 INZ(*zeros)
003100010330     D recko           s            150    INZ(*blanks)
003200011113     D depcmd          s            150    INZ(*blanks)
003300031215     D contaRec        s              9  0
003400010330     D*------------------
003500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003600010330     D*------------------
003700010330     D WLBDA8          DS                  INZ
003800010330     D  G08DAT                 1      8  0
003900010330     D  G08INV                 9     16  0
004000010330     D  G08ERR                17     17
004100010330     D  G08TGI                18     22  0
004200000313     D*
004300010330
004400010330
004500010330
004600990915     C                   time                    wrkdata
004700990915     C                   time                    wrkora
004800000913     C                   reset                   rrnum
004900990921     C                   reset                   esito
005000990921     C                   reset                   wrkesito
005100010601     C*
005200010601     C                   exsr      opeini
005300010605     C                   exsr      rwvab
005400010601     C*
005500010601     C                   seton                                        lr
005600010601
005700010601
005800010601
005900010601
006000010601     C*--------------------------------------------------------
006100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006200010601     C*--------------------------------------------------------
006300010601     C     PREELA        BEGSR
006400010601     C*
006500010601     C* SE OCCORRE SPEDIRE IN FILIALE
006600010601     C                   if        invfil <> *zeros and
006700010601     C                             flgGiro = '0'
006800010601     C*
006900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007000010601     C                   eval      flgGiro = '1'
007100000724     C*
007200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007400010601     C     invfil        chain     azorg01l
007500000724     C                   if        %found
007600000724     C                   movel(p)  CMD(1)        depcmd
007700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007800000724     C*
007900000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008000011113     C                   Z-ADD     150           LENGH            15 5
008100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008200000724     C                   PARM                    depcmd
008300000724     C                   PARM                    LENGH
008400000724     C*
008500000724     C                   endif
008600000616     C*
008700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008800000616     C                   movel(p)  CMD(2)        depcmd
008900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009000000616     C*
009100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009200011113     C                   Z-ADD     150           LENGH            15 5
009300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009400000616     C                   PARM                    depcmd
009500000616     C                   PARM                    LENGH
009600000613     C*
009700031215     C                   if        not %open(titv81ps)
009800031215     C                   open      titv81ps
009900000616     C                   except    testdett
010000000613     C                   endif
010100010601     C*
010200010601     C                   endif
010300010601     C*
010400010601     C                   ENDSR
010500010601     C***
010600010601
010700010601
010800010601
010900010601
011000010601
011100010601
011200010601
011300010601     C*--------------------------------------------------------
011400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011500010601     C*--------------------------------------------------------
011600010601     C     ENDELA        BEGSR
011700990915     C*
011800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011900050201     C*
012000050201     C* Effettuo lancio TISI95 solo x chiusura
012100050201     C                   CLEAR                   TISI95DS
012200050201     C                   EVAL      I95TLA = 'C'
012300050201     C                   CALL      'TISI95R'
012400050201     C                   PARM                    TISI95DS
012500000613     C*
012600031215     C                   if        %open(titv81ps)
012700000616     C                   except    findett
012800031215     C                   close     titv81ps
012900000613     C                   endif
013000000616     C*
013100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013200011113     C                   Z-ADD     150           LENGH            15 5
013300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013400000616     C                   PARM                    CMD(3)
013500000616     C                   PARM                    LENGH
013600000616     C*
013700010601     C                   ENDSR
013800010601     C***
013900010601
014000010601
014100010601
014200010601
014300000613
014400000613     C*--------------------------------------------------------
014500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014600000613     C*--------------------------------------------------------
014700000621     C     STPR          BEGSR
014800000613     C*
014900031215     C                   if        not %open(titv81p)
015000031215     C                   open      titv81p
015100990915     C                   endif
015200990915     C*
015300990915     C                   except    riepilogo
015400990915     C*
015500031215     C                   if        %open(titv81p)
015600031215     C                   close     titv81p
015700990914     C                   endif
015800000613     C*
015900000613     C                   ENDSR
016000000613     C***
016100000613
016200010330
016300010601
016400010601
016500010601
016600010330     C*--------------------------------------------------------
016700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016800010330     C*--------------------------------------------------------
016900010605     C     RWVAB         BEGSR
017000010330     C*
017100010330     C                   if        not %open(tivin00r)
017200010330     C                   open      tivin00r
017300010330     C                   endif
017400021025     C                   if        not %open(fivabwwr)
017500021025     C                   open      fivabwwr
017600010330     C                   endif
017700010330     C*
017800010604     C                   clear                   §CTROK
017900010604     C                   clear                   §CTRMO
018000010604     C                   clear                   §CTRNO
018100010330     C*
018200010330     C                   DO        *HIVAL
018300010330     C*
018400010330     C                   READ      tivin00r                               70
018500031215     C*
018600031215     C* Incremento il contatore record letti
018700031215     C                   ADD       1             contaRec
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
020700031215     C* Scarto i primi 6 record del file (in quanto intestazione della stampa) e l'ultimo record
020800031215     C* dei totali
020900031215     C                   if        contaRec > 6 and
021000031215     C                             %subst(vindta:1:7) <> 'Total :'
021100010330     C                   exsr      inzvar
021200010330     C                   exsr      defcam
021300010605     C                   exsr      impvab
021400010601     C*
021500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021600010605     C                   eval      depfil = VABLNP
021700010601     C                   exsr      repfil
021800010601     C                   if        depfil = invfil
021900021025     C                   if        vlrpoi = 999
022000021025     C                   MOVE(P)   invfil        VABFGS
022100021025     C                   else
022200021025     C                   MOVE(P)   vlrpoi        VABFGS
022300021025     C                   endif
022400010601     C*
022500010601     C                   exsr      PREELA
022600010601     C*
022700010604     C* Ebbene...
022800010604     C*
022900010604     C  N31              ADD       1             §CTROK            7 0
023000010604     C   32              ADD       1             §CTRMO            7 0
023100010604     C   31              ADD       1             §CTRNO            7 0
023200021025     C  N31              WRITE     FIVAB000
023300010604     C*
023400010604     C                   if        *in31 = *off and
023500010604     C                             *in32 = *off
023600010604     C                   eval      vinflg = '1'
023700010604     C                   else
023800010604     C                   eval      recko = vindta
023900010604     C                   except    rigadett
024000010604     C                   eval      vinflg = '2'
024100010604     C                   endif
024200010604     C                   endif
024300031215     C                   endif
024400010601     C*
024500010604     C                   endif
024600010604     C*
024700010330     C                   else
024800010330     C                   eval      vinflg = '1'
024900010330     C                   endif
025000010601     C*
025100010601     C  N70              update    tivin000
025200010330     C*
025300010330     C  N70              ENDdo
025400010601     C
025500010601     C*
025600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025700010601     C                   if        cntNonEl = *zeros or
025800010601     C                             flgMulti = '0'
025900010330     C* Se non ci sono record con errori ...
026000010601     C                   if        §ctrno = 0 and
026100010604     C                             §ctrmo = 0 and
026200010601     C                             flgStato <> '2'
026300010330     C* ... restituisco esito OK.
026400010330     C                   eval      wrkesito = '0'
026500010330     C                   else
026600010330     C                   if        §ctrok > 0
026700010330     C                   eval      wrkesito = '1'
026800010330     C                   else
026900010615     C                   if        flgOk = '0'
027000010615     C                   eval      wrkesito = '2'
027100010615     C                   else
027200010615     C                   eval      wrkesito = '6'
027300010615     C                   endif
027400010330     C                   endif
027500010330     C                   endif
027600010601     C                   else
027700010601     C                   eval      wrkesito = '9'
027800010601     C                   endif
027900010330     C*
028000010330     C                   if        %open(tivin00r)
028100010330     C                   close     tivin00r
028200010330     C                   endif
028300021025     C                   if        %open(fivabwwr)
028400021025     C                   close     fivabwwr
028500010330     C                   endif
028600010601     C*
028700010601     C                   if        vlrpoi <> 999
028800010601     C                   eval      invfil = vlrpoi
028900010601     C                   endif
029000010330     C*
029100010330     C                   if        §ctrok > 0
029200010601     C                             and invfil > *zeros
029300010330     C                   exsr      invio
029400010330     C                   endif
029500010601     C*
029600010618     C                   if        flgGiro = '1'
029700010601     C                   exsr      endela
029800010618     C                   endif
029900010330     C*
030000010330     C                   ENDSR
030100010330     C***
030200010601
030300010601
030400010601
030500010330     C*----------------------------------------------------*
030600010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030700010330     C*----------------------------------------------------*
030800010330     C     INZVAR        BEGSR
030900010330     C*
031000020204     C                   Z-ADD     *zeros        Num5_0            5 0
031100030606     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031200030606     C                   MOVEL     '0'           FlgCAS            1
031300010330     C*
031400010330     C                   ENDSR
031500010330     C*----------------------------------------------------*
031600010330     C*  IMPOSTAZIONE CAMPI COSTANTI
031700010330     C*----------------------------------------------------*
031800010330     C     DEFCAM        BEGSR
031900010330     C*
032000020204     C* Inizializzo il buffer del record da scrivere
032100021025     C                   CLEAR                   FIVAB000
032200020204     C* Imposto i valori di default...
032300050419     C***                EVAL      VABCCM = 0052561
032400050419     C                   EVAL      VABCCM = 0053279
032500031006     C                   EVAL      VABLNP = 005
032600031006     C                   EVAL      VABCTR = 300
032700031215     C                   EVAL      VABPKB = 1
032800031216     C                   EVAL      VABNCL = 1
032900020204     C* ... e poi verifico se sono stati passati come parametri
033000020204     C                   IF        vlrppt > *blanks
033100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033200020204     C                   EXSR      CHKNUM
033300020204     C                   IF        PiInt=*on
033400020204     C                   Z-ADD     PiVal         VABCCM
033500020204     C                   ENDIF
033600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
033700020204     C                   EXSR      CHKNUM
033800020204     C                   IF        PiInt=*on
033900020204     C                   Z-ADD     PiVal         VABLNP
034000020204     C                   ENDIF
034100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
034200020204     C                   EXSR      CHKNUM
034300020204     C                   IF        PiInt=*on
034400020204     C                   Z-ADD     PiVal         VABCTR
034500020204     C                   ENDIF
034600020204     C                   ENDIF
034700020204     C*
034800010330     C                   ENDSR
034900010607     C*----------------------------------------------------*
035000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
035100010607     C*----------------------------------------------------*
035200010607     C     IMPVAB        BEGSR
035300010607     C*
035400010607     C                   SETOFF                                       3132
035500030515     C*
035600010607     C                   MOVEL     datcor        VABAAS
035700010607     C                   MOVE      datcor        VABMGS
035800010607     C*
035900010607     C* Reperimento campi ALFA
036000010607     C*
036100010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
036200031215     C                   EVAL      VABRSD=%trim(%subst(vindta:1:31))
036300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
036400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
036500020117     C* ==
036600031215     C                   EVAL      VABIND=%trim(%subst(vindta:32:27))
036700031006     C*
036800031006     C* Essendo il campo località passato dal cliente nella forma:
036900031215     C* località (provincia) => eseguo alcune considerazione; x cui:
037000031215     C                   Z-ADD     *zeros        wPos              3 0
037100031215     C                   MOVEL     *blanks       wStringa         20
037200031215     C                   EVAL      wStringa=%trim(%subst(vindta:59:20))
037300031215     C                   EVAL      wPos=%scan('(':wStringa)
037400031006     C                   IF        wPos>*zeros
037500031006     C                   EVAL      VABLOD=%trim(%subst(wStringa:1:wPos-1))
037600031215     C                   ELSE
037700031215     C                   EVAL      VABLOD=%trim(wStringa)
037800031006     C                   ENDIF
037900031006     C*
038000031215     C                   EVAL      VABRMA=%trim(%subst(vindta:91:6))
038100010607     C*
038200010607     C* Reperimento campi NUMERICI
038300020604     C*
038400030515     C* CAD
038500031215     C                   EVAL      PiStr=%trim(%subst(vindta:82:5))
038600010607     C                   EXSR      CHKNUM
038700010607     C                   IF        PiInt=*on
038800020204     C                   Z-ADD     PiVal         Num5_0
038900020204     C                   MOVEL(p)  Num5_0        VABCAD
039000010607     C                   ELSE
039100010607     C                   SETON                                        32
039200020204     C                   EVAL      VABCAD = *zeros
039300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039400020204     C                             + ' ' + 'VABCAD'
039500010607     C                   ENDIF
039600031215     C* Reperisco la provincia dal CAP e dalla località
039700031215     C                   IF        VABCAD <> *zeros
039800031215     C                   CLEAR                   TISI95DS
039900031215     C                   EVAL      I95TCN = '3'
040000031215     C                   Z-ADD     datcor        I95DAT
040100031215     C                   EVAL      I95CAP = VABCAD
040200031215     C                   EVAL      I95LOC = VABLOD
040300031215     C                   CALL      'TISI95R'
040400031215     C                   PARM                    TISI95DS
040500031215     C                   EVAL      VABPRD = O95PRV
040600031215     C                   ENDIF
040700030515     C* RMN/NSP
040800031215     C                   EVAL      PiStr=%trim(%subst(vindta:91:6))
040900011023     C                   EXSR      CHKNUM
041000011023     C                   IF        PiInt=*on
041100030225     C                   Z-ADD     PiVal         VABRMN
041200031013     C                   MOVE(P)   VABRMN        VABNSP
041300011023     C                   ELSE
041400020205     C                   EVAL      VABRMN = 1
041500011023     C                   ENDIF
041600031215     C* CAS
041700031216     C                   IF        %trim(%subst(vindta:101:12)) = 'CONTRASSEGNO'
041800031006     C                   EVAL      FlgCAS = '1'
041900031006     C                   ENDIF
042000030929     C*
042100030929     C* Considerazioni finali su CBO/CAS
042200030929     C                   IF        FlgCAS = '1'
042300030929     C                   EVAL      VABCBO = '4'
042400030929     C                   ELSE
042500030929     C                   EVAL      VABCBO = '1'
042600030929     C                   ENDIF
042700020204     C*
042800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042900020204     C                   EXSR      CHKIMPDIV
043000020204     C*
043100010607     C                   ENDSR
043200010607     C*----------------------------------------------------*
043300010601
043400020204
043500020204     C*----------------------------------------------------*
043600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
043700020204     C*----------------------------------------------------*
043800020204     C     CHKIMPDIV     BEGSR
043900020204     C*
044000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
044100020204     C                   Z-ADD     *zeros        wrkDec            9 9
044200020204     C*
044300020204     C* Come prima cosa effettuo considerazioni sulla divisa
044400020204     C                   IF        vabIAS > *zeros
044500020204     C                   IF        vabVAS <> 'EUR'
044600020204     C                   EVAL      vabVAS =  'ITL'
044700020204     C                   ENDIF
044800020204     C                   ENDIF
044900020204     C*
045000020204     C                   IF        vabCAS > *zeros
045100020204     C                   IF        vabVCA <> 'EUR'
045200020204     C                   EVAL      vabVCA =  'ITL'
045300020204     C                   ENDIF
045400020204     C                   ENDIF
045500020204     C*
045600020204     C                   IF        vabVMD > *zeros
045700020204     C                   IF        vabVAD <> 'EUR'
045800020204     C                   EVAL      vabVAD =  'ITL'
045900020204     C                   ENDIF
046000020204     C                   ENDIF
046100020204     C*
046200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
046300020204     C                   Z-ADD     vabIAS        wrkDec
046400020204     C                   IF        wrkDec > *zeros
046500020204     C                   IF        vabVAS = 'ITL'
046600020204     C                   EVAL      vabIAS = *zeros
046700020204     C                   ENDIF
046800020204     C                   ENDIF
046900020204     C*
047000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
047100020204     C                   Z-ADD     vabCAS        wrkDec
047200020204     C                   IF        wrkDec > *zeros
047300020204     C                   IF        vabVCA = 'ITL'
047400020204     C                   EVAL      vabCAS = *zeros
047500020204     C                   ENDIF
047600020204     C                   ENDIF
047700020204     C*
047800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
047900020204     C                   Z-ADD     vabVMD        wrkDec
048000020204     C                   IF        wrkDec > *zeros
048100020204     C                   IF        vabVAD = 'ITL'
048200020204     C                   EVAL      vabVMD = *zeros
048300020204     C                   ENDIF
048400020204     C                   ENDIF
048500020204     C*
048600020204     C                   ENDSR
048700020204     C***
048800020204
048900010330
049000010330
049100010330     C*----------------------------------------------------*
049200010330     C*  CONTROLLO NUMERICITA' CAMPI
049300010330     C*----------------------------------------------------*
049400010330     C     CHKNUM        BEGSR
049500010330     C*
049600010606     C                   IF        PiDecChr = *blanks
049700030606     C                   EVAL      PiDecChr = ','
049800010606     C                   ENDIF
049900010606     C*
050000010606     C                   CALL(e)   'ISNUMERIC'
050100010330     C                   PARM                    PiStr            30
050200010606     C                   PARM                    PiDecChr          1
050300010330     C                   PARM      *ZEROS        PiVal            30 9
050400010330     C                   PARM      '0'           PiInt             1
050500010330     C                   PARM      '0'           PiNum             1
050600010330     C                   IF        %error
050700010606     C                   EVAL      PiNum=*off
050800010330     C                   ENDIF
050900010330     C*
051000010330     C                   ENDSR
051100010330     C***
051200010330
051300010601
051400010601
051500010601
051600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
051700010601     C     repfil        BEGSR
051800010601     C*
051900010601     C                   if        invfil = *zeros and
052000010601     C                             depfil > *zeros and
052100010629     C                             (vinflg = *blanks or
052200010629     C                              vinflg = *zeros)
052300010601     C
052400010601     C                   eval      invfil = depfil
052500010601     C                   endif
052600010601     C*
052700010601     C                   if        depfil <> invfil and
052800010601     C                             invfil > *zeros
052900010601     C                   eval      flgMulti = '1'
053000010601     C                   if        vinflg = *blanks
053100010601     C                   add       1             cntNonEl
053200010601     C                   endif
053300010601     C                   endif
053400010601     C*
053500010601     C                   if        vinflg = '2'
053600010601     C                   eval      flgStato = '2'
053700010601     C                   endif
053800010601     C*
053900010601     C                   ENDSR
054000010601     C***
054100010601
054200010601
054300010601
054400010330
054500010330
054600010330
054700990920      /TITLE Invio dei dati al punto operativo.
054800000613     C     invio         BEGSR
054900990920     C*
055000990920     C                   reset                   dscmz
055100010601     C                   move      invfil        cmzdst
055200990920     C                   eval      cmzfld = vlrfou
055300990920     C                   eval      cmzmbd = vlrhdl
055400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
055500000710     C                   if        prmfir = *blanks
055600021025     C                   eval      cmzfla = 'FIVAB00F'
055700021025     C                   eval      cmzmba = 'FIVAB00F'
055800000710     C                   else
055900000710     C                   eval      cmzfla = prmfir
056000000710     C                   eval      cmzmba = prmfir
056100000710     C                   endif
056200990920     C                   eval      cmznrr = *zeros
056300990920     C                   move      §ctrok        cmznrr
056400021018     C                   eval      cmzlba = vlrfl1
056500990920     C                   call(e)   'TIS711C'
056600990920     C                   parm                    dscmz
056700990921     C                   parm      *blanks       esito
056800990920     C                   if        %error
056900990920     C                             or cmzerr = '1'
057000990921     C                             or esito  = '1'
057100000710     C                   eval      wrkesito = '3'
057200990920     C                   endif
057300990920     C*
057400000613     C                   ENDSR
057500990910
057600010601
057700010601
057800010601
057900010601
058000010601      /TITLE Invio dei dati al punto operativo.
058100010601     C     opeini        BEGSR
058200010601     C*
058300010601     C* Inizializzo flag e contatori operativi
058400010601     C                   movel     '0'           flgGiro           1
058500010601     C                   movel     '0'           flgMulti          1
058600010601     C                   movel     '1'           flgStato          1
058700010615     C                   movel     '0'           flgOk             1
058800010601     C                   z-add     *zeros        cntNonEl         10 0
058900010601     C                   z-add     *zeros        depfil            3 0
059000010601     C                   z-add     *zeros        invfil            3 0
059100010601     C*
059200010601     C                   ENDSR
059300010601     C***
059400010601
059500010601
059600010601
059700010330
059800010330
059900000613     C     *inzsr        BEGSR
060000990910     C*
060100990910     C     *entry        plist
060200990920     C                   parm                    tivlrds
060300990921     C                   parm      wrkesito      esito
060400000724     C                   parm                    prmlit
060500000710     C                   parm                    prmfir
060600010330     C*
060700010330     C* CALCOLA LA DATA CORRENTE
060800010330     C                   time                    wn14             14 0
060900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061000010330     C                   z-add     wn8           g08dat
061100010330     C                   z-add     *zeros        g08inv
061200010330     C                   movel     '0'           g08err
061300010330     C                   call      'XSRDA8'
061400010330     C                   parm                    wlbda8
061500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061600000613     C*
061700000613     C                   ENDSR
061800000613     C***
061900990908
062000031215     Otitv81p   E            riepilogo         2
062100990915     O                                              'Upload via Internet'
062200990915     O                                           +1 'Traduzione TIVIN00R -'
062300021025     O                                           +1 'FIVABWWR'
062400990915     O                                           +1 'Testate bolle'
062500990915     O          E            riepilogo   2
062600990915     O                       wrkdata
062700990915     O                       wrkora              +1
062800990915     O                       procname            +1
062900990915     O          E            riepilogo   2
063000990915     O                                              'Cliente..................:'
063100010605     O                       VABCCM        z     +1
063200990915     O          E            riepilogo   2
063300990920     O                                              'Riferimento Strategi.....:'
063400990920     O                       vlrhdl              +1
063500990915     O          E            riepilogo   2
063600990915     O                                              'Giusti...................:'
063700971022     O                       §CTROK        2   +  1
063800990915     O          E            riepilogo   2
063900990915     O                                              'Sbagliati e corretti.....:'
064000971022     O                       §CTRMO        2   +  1
064100990915     O          E            riepilogo   2
064200990915     O                                              'Sbagliati e scartati.....:'
064300971022     O                       §CTRNO        2   +  1
064400000613
064500031215     Otitv81ps  E            testdett          2
064600000613     O                                              'Upload via Internet'
064700000613     O                                           +1 'Traduzione TIVIN00R -'
064800021025     O                                           +1 'FIVABWWR'
064900000613     O                                           +1 'Testate bolle'
065000000616     O          E            testdett    3
065100000613     O                                           +2 'N° rec'
065200000613     O                                           +3 'Anteprima contenuto'
065300000616     O          E            rigadett    2
065400000613     O                       rrnum               +2
065500000621     O                       recko               +3
065600000616     O          E            findett     2
065700000613     O                       wrkdata
065800000613     O                       wrkora              +1
065900000613     O                       procname            +1
066000000616     O          E            findett     2
066100000613     O                                              'Cliente..................:'
066200010605     O                       VABCCM        z     +1
066300000616     O          E            findett     2
066400000613     O                                              'Riferimento Strategi.....:'
066500000613     O                       vlrhdl              +1
066600000616     O          E            findett     2
066700000613     O                                              'Giusti...................:'
066800000613     O                       §CTROK        2   +  1
066900000616     O          E            findett     2
067000000613     O                                              'Sbagliati e corretti.....:'
067100000613     O                       §CTRMO        2   +  1
067200000616     O          E            findett     2
067300000613     O                                              'Sbagliati e scartati.....:'
067400000613     O                       §CTRNO        2   +  1
067500000613** CMD - COMANDI CL
067600031215OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV81P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
067700031215OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV81PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
067800031215DLTOVR FILE(TITV81P TITV81PS) LVL(*)
067900010606
068000000613
