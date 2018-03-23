000100060901      /TITLE Upload via Internet: traduzione in FIVABWWR. (x SGE SYSCOM Cod. 2022245)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021014     FFIVABwwr  O    E             DISK    usropn
000700060901     Ftitvu7p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900060901     Ftitvu7ps  O    f  198        PRINTER usropn
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
004200010330
004300010330
004400990915     C                   time                    wrkdata
004500990915     C                   time                    wrkora
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900010601     C*
005000010601     C                   exsr      opeini
005100010605     C                   exsr      rwvab
005200010601     C*
005300010601     C                   seton                                        lr
005400010601
005500010601
005600010601
005700010601
005800010601     C*--------------------------------------------------------
005900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006000010601     C*--------------------------------------------------------
006100010601     C     PREELA        BEGSR
006200010601     C*
006300010601     C* SE OCCORRE SPEDIRE IN FILIALE
006400010601     C                   if        invfil <> *zeros and
006500010601     C                             flgGiro = '0'
006600010601     C*
006700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006800010601     C                   eval      flgGiro = '1'
006900000724     C*
007000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007200010601     C     invfil        chain     azorg01l
007300000724     C                   if        %found
007400000724     C                   movel(p)  CMD(1)        depcmd
007500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007600000724     C*
007700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007800011113     C                   Z-ADD     150           LENGH            15 5
007900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008000000724     C                   PARM                    depcmd
008100000724     C                   PARM                    LENGH
008200000724     C*
008300000724     C                   endif
008400000616     C*
008500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008600000616     C                   movel(p)  CMD(2)        depcmd
008700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008800000616     C*
008900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009000011113     C                   Z-ADD     150           LENGH            15 5
009100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009200000616     C                   PARM                    depcmd
009300000616     C                   PARM                    LENGH
009400000613     C*
009500060901     C                   if        not %open(titvu7ps)
009600060901     C                   open      titvu7ps
009700000616     C                   except    testdett
009800000613     C                   endif
009900010601     C*
010000010601     C                   endif
010100010601     C*
010200010601     C                   ENDSR
010300010601     C***
010400010601
010500010601
010600010601
010700010601     C*--------------------------------------------------------
010800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010900010601     C*--------------------------------------------------------
011000010601     C     ENDELA        BEGSR
011100990915     C*
011200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011300000613     C*
011400060901     C                   if        %open(titvu7ps)
011500000616     C                   except    findett
011600060901     C                   close     titvu7ps
011700000613     C                   endif
011800000616     C*
011900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012000011113     C                   Z-ADD     150           LENGH            15 5
012100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012200000616     C                   PARM                    CMD(3)
012300000616     C                   PARM                    LENGH
012400000616     C*
012500010601     C                   ENDSR
012600010601     C***
012700010601
012800010601
012900000613
013000000613     C*--------------------------------------------------------
013100000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013200000613     C*--------------------------------------------------------
013300000621     C     STPR          BEGSR
013400000613     C*
013500060901     C                   if        not %open(titvu7p)
013600060901     C                   open      titvu7p
013700990915     C                   endif
013800990915     C*
013900990915     C                   except    riepilogo
014000990915     C*
014100060901     C                   if        %open(titvu7p)
014200060901     C                   close     titvu7p
014300990914     C                   endif
014400000613     C*
014500000613     C                   ENDSR
014600000613     C***
014700000613
014800010601
014900010601
015000010330     C*--------------------------------------------------------
015100040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015200010330     C*--------------------------------------------------------
015300010605     C     RWVAB         BEGSR
015400010330     C*
015500010330     C                   if        not %open(tivin00r)
015600010330     C                   open      tivin00r
015700010330     C                   endif
015800021014     C                   if        not %open(fivabwwr)
015900021014     C                   open      fivabwwr
016000010330     C                   endif
016100010330     C*
016200010604     C                   clear                   §CTROK
016300010604     C                   clear                   §CTRMO
016400010604     C                   clear                   §CTRNO
016500010330     C*
016600010330     C                   DO        *HIVAL
016700010330     C*
016800010330     C                   READ      tivin00r                               70
016900010618     C*
017000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017100010618     C                   if        vinflg = '1'
017200010618     C                   eval      flgOk = '1'
017300010618     C                   endif
017400010618     C*
017500010330     C                   if        vindta > *blanks
017600010330     C                   add       1             rrnum
017700010330     C*
017800010601     C                   if        *in70 = *off and
017900010330     C                             (vinflg = *blanks
018000010330     C                              or vinflg = '0'
018100010330     C                              or vinflg = '2')
018200010330     C*
018300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018400010711     C                   if        vinflg = *blanks or vinflg = '0'
018500010711     C                   clear                   vinmsg
018600010711     C                   endif
018700010601     C*
018800060901     C                   exsr      inzvar
018900060901     C                   exsr      defcam
019000010605     C                   exsr      impvab
019100010601     C*
019200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019300010605     C                   eval      depfil = VABLNP
019400010601     C                   exsr      repfil
019500010601     C                   if        depfil = invfil
019600021025     C                   if        vlrpoi = 999
019700021025     C                   MOVE(P)   invfil        VABFGS
019800021025     C                   else
019900021025     C                   MOVE(P)   vlrpoi        VABFGS
020000021025     C                   endif
020100010601     C*
020200010601     C                   exsr      PREELA
020300060901     C*
020400060901     C* FORZATURA PARTICOLARE: se colli e peso nn sono valorizzati nn è 1 errore ma
020500060901     C* vanno semplicemente ignorati!!!!  Così garantisce il P.O.
020600060901     C                   if        vabNCL = *zeros AND vabPKB = *zeros
020700060901     C                   else
020800010601     C*
020900010604     C* Ebbene...
021000010604     C*
021100010604     C  N31              ADD       1             §CTROK            7 0
021200010604     C   32              ADD       1             §CTRMO            7 0
021300010604     C   31              ADD       1             §CTRNO            7 0
021400020205     C*
021500040301     C* Scarico il buffer del FIVAB
021600021014     C  N31              WRITE     FIVAB000
021700010604     C*
021800060901     C                   endif
021900020722     C                   endif
022000020722     C*
022100010604     C                   if        *in31 = *off and
022200010604     C                             *in32 = *off
022300010604     C                   eval      vinflg = '1'
022400010604     C                   else
022500010604     C                   eval      recko = vindta
022600010604     C                   except    rigadett
022700010604     C                   eval      vinflg = '2'
022800010604     C                   endif
022900010604     C                   endif
023000010604     C*
023100010330     C                   else
023200010330     C                   eval      vinflg = '1'
023300010330     C                   endif
023400010601     C*
023500010601     C  N70              update    tivin000
023600010330     C*
023700010330     C  N70              ENDdo
023800010601     C
023900010601     C*
024000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024100010601     C                   if        cntNonEl = *zeros or
024200010601     C                             flgMulti = '0'
024300010330     C* Se non ci sono record con errori ...
024400010601     C                   if        §ctrno = 0 and
024500010604     C                             §ctrmo = 0 and
024600010601     C                             flgStato <> '2'
024700010330     C* ... restituisco esito OK.
024800010330     C                   eval      wrkesito = '0'
024900010330     C                   else
025000010330     C                   if        §ctrok > 0
025100010330     C                   eval      wrkesito = '1'
025200010330     C                   else
025300010615     C                   if        flgOk = '0'
025400010615     C                   eval      wrkesito = '2'
025500010615     C                   else
025600010615     C                   eval      wrkesito = '6'
025700010615     C                   endif
025800010330     C                   endif
025900010330     C                   endif
026000010601     C                   else
026100010601     C                   eval      wrkesito = '9'
026200010601     C                   endif
026300010330     C*
026400010330     C                   if        %open(tivin00r)
026500010330     C                   close     tivin00r
026600010330     C                   endif
026700021014     C                   if        %open(fivabwwr)
026800021014     C                   close     fivabwwr
026900010330     C                   endif
027000010601     C*
027100010601     C                   if        vlrpoi <> 999
027200010601     C                   eval      invfil = vlrpoi
027300010601     C                   endif
027400010330     C*
027500010330     C                   if        §ctrok > 0
027600010601     C                             and invfil > *zeros
027700010330     C                   exsr      invio
027800010330     C                   endif
027900010601     C*
028000010618     C                   if        flgGiro = '1'
028100010601     C                   exsr      endela
028200010618     C                   endif
028300010330     C*
028400010330     C                   ENDSR
028500010330     C***
028600010330
028700010601
028800010601
028900010601
029000010601
029100010330     C*----------------------------------------------------*
029200020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029300010330     C*----------------------------------------------------*
029400010330     C     INZVAR        BEGSR
029500010330     C*
029600020204     C                   Z-ADD     *zeros        Num5_0            5 0
029700020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029800020322     C                   MOVEL     '0'           FlgCAS            1
029900010330     C*
030000010330     C                   ENDSR
030100010330     C*----------------------------------------------------*
030200020722     C*  IMPOSTAZIONE CAMPI COSTANTI
030300010330     C*----------------------------------------------------*
030400010330     C     DEFCAM        BEGSR
030500010330     C*
030600020204     C* Inizializzo il buffer del record da scrivere
030700021014     C                   CLEAR                   FIVAB000
030800020204     C* Imposto i valori di default...
030900060901     C                   EVAL      VABCCM = 2022245
031000060901     C                   EVAL      VABLNP = 202
031100060901     C                   EVAL      VABCTR = 000
031200060901     C                   EVAL      VABTSP = 'C'
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
034600060720     C*
034700060720     C* Reperimento campi NUMERICI
034800060720     C                   MOVEL     DATCOR        VABAAS
034900060720     C                   MOVE      DATCOR        VABMGS
035000010607     C*
035100010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
035200060901     C                   EVAL      VABRSD=%trim(%subst(vindta:3:35))
035300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035500020117     C* ==
035600060901     C                   EVAL      VABRD2=%trim(%subst(vindta:38:35))
035700060901     C                   EVAL      VABIND=%trim(%subst(vindta:73:35))
035800060901     C                   EVAL      VABLOD=%trim(%subst(vindta:117:35))
035900060901     C                   EVAL      VABPRD=%trim(%subst(vindta:152:2))
036000060901     C                   EVAL      VABRMA=%trim(%subst(vindta:185:15))
036100060901     C                   EVAL      VABCBO=%trim(%subst(vindta:1:1))
036200060720     C* CAD
036300060901     C                   EVAL      PiStr=%trim(%subst(vindta:108:5))
036400060720     C                   EXSR      CHKNUM
036500060720     C                   IF        PiInt=*on
036600060720     C                   Z-ADD     PiVal         Num5_0
036700060720     C                   MOVEL(p)  Num5_0        VABCAD
036800060720     C                   ELSE
036900060720     C                   SETON                                        32
037000060720     C                   EVAL      VABCAD = *zeros
037100060720     C                   EVAL      vinmsg = %trimr(vinmsg)
037200060720     C                             + ' ' + 'VABCAD'
037300060720     C                   ENDIF
037400040420     C* NSP/RMN
037500090122     C                   EVAL      PiStr=%trim(%subst(vindta:185+1:15-1))
037600020304     C                   EXSR      CHKNUM
037700020304     C                   IF        PiInt=*on
037800040301     C                   Z-ADD     PiVal         VABNSP
037900040301     C                   Z-ADD     PiVal         VABRMN
038000020304     C                   ELSE
038100020722     C                   SETON                                        31
038200020304     C                   Z-ADD     *zeros        VABNSP
038300040301     C                   Z-ADD     *zeros        VABRMN
038400020304     C                   EVAL      vinmsg = %trimr(vinmsg)
038500040301     C                             + ' ' + 'VABNSP VABRMN'
038600020304     C                   ENDIF
038700040420     C* NCL
038800060901     C                   EVAL      PiStr=%trim(%subst(vindta:154:5))
038900010607     C                   EXSR      CHKNUM
039000010607     C                   IF        PiInt=*on
039100010607     C                   Z-ADD     PiVal         VABNCL
039200010607     C                   ELSE
039300010607     C                   SETON                                        32
039400010607     C                   Z-ADD     *zeros        VABNCL
039500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039600010607     C                             + ' ' + 'VABNCL'
039700010607     C                   ENDIF
039800040420     C* PKB
039900060901     C                   EVAL      PiStr=%trim(%subst(vindta:160:8))
040000010607     C                   EXSR      CHKNUM
040100010607     C                   IF        PiNum=*on
040200040420     C                   Z-ADD     PiVal         VABPKB
040300010607     C                   ELSE
040400010607     C                   SETON                                        32
040500010607     C                   Z-ADD     *zeros        VABPKB
040600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040700010607     C                             + ' ' + 'VABPKB'
040800010607     C                   ENDIF
040900060901     C* CAS
041000060901     C                   IF        %subst(vindta:169:14) <> '0000000000,000'
041100060901     C                   EVAL      FlgCAS = '1'
041200060720     C                   EVAL      VABVAS='EUR'
041300060901     C                   EVAL      VABTIC=%trim(%subst(vindta:183:2))
041400060901     C                   EVAL      PiStr=%trim(%subst(vindta:169:14))
041500060720     C                   EXSR      CHKNUM
041600060720     C                   IF        PiNum=*on
041700060901     C                   Z-ADD     PiVal         VABCAS
041800060720     C                   ELSE
041900060720     C                   SETON                                        32
042000060901     C                   Z-ADD     *zeros        VABCAS
042100060720     C                   EVAL      vinmsg = %trimr(vinmsg)
042200060901     C                             + ' ' + 'VABCAS'
042300060720     C                   ENDIF
042400060720     C                   ENDIF
042500020322     C*
042600020322     C* Considerazioni finali su CBO/CAS
042700020322     C                   IF        FlgCAS = '1'
042800040713     C                   IF        VABCBO = '1'
042900020322     C                   EVAL      VABCBO = '4'
043000040713     C                   ENDIF
043100040713     C                   IF        VABCBO = '2'
043200040713     C                   EVAL      VABCBO = '6'
043300040713     C                   ENDIF
043400020322     C                   ENDIF
043500020204     C*
043600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043700020204     C                   EXSR      CHKIMPDIV
043800020204     C*
043900010607     C                   ENDSR
044000010607     C*----------------------------------------------------*
044100010601
044200020204
044300020204     C*----------------------------------------------------*
044400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
044500020204     C*----------------------------------------------------*
044600020204     C     CHKIMPDIV     BEGSR
044700020204     C*
044800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
044900020204     C                   Z-ADD     *zeros        wrkDec            9 9
045000020204     C*
045100020204     C* Come prima cosa effettuo considerazioni sulla divisa
045200020204     C                   IF        vabIAS > *zeros
045300020204     C                   IF        vabVAS <> 'EUR'
045400020204     C                   EVAL      vabVAS =  'ITL'
045500020204     C                   ENDIF
045600020204     C                   ENDIF
045700020204     C*
045800020204     C                   IF        vabCAS > *zeros
045900020204     C                   IF        vabVCA <> 'EUR'
046000020204     C                   EVAL      vabVCA =  'ITL'
046100020204     C                   ENDIF
046200020204     C                   ENDIF
046300020204     C*
046400020204     C                   IF        vabVMD > *zeros
046500020321     C                   IF        vabVAD <> 'EUR'
046600020204     C                   EVAL      vabVAD =  'ITL'
046700020204     C                   ENDIF
046800020204     C                   ENDIF
046900020204     C*
047000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
047100020204     C                   Z-ADD     vabIAS        wrkDec
047200020204     C                   IF        wrkDec > *zeros
047300020204     C                   IF        vabVAS = 'ITL'
047400020204     C                   EVAL      vabIAS = *zeros
047500020204     C                   ENDIF
047600020204     C                   ENDIF
047700020204     C*
047800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
047900020204     C                   Z-ADD     vabCAS        wrkDec
048000020204     C                   IF        wrkDec > *zeros
048100020204     C                   IF        vabVCA = 'ITL'
048200020204     C                   EVAL      vabCAS = *zeros
048300020204     C                   ENDIF
048400020204     C                   ENDIF
048500020204     C*
048600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048700020204     C                   Z-ADD     vabVMD        wrkDec
048800020204     C                   IF        wrkDec > *zeros
048900020204     C                   IF        vabVAD = 'ITL'
049000020204     C                   EVAL      vabVMD = *zeros
049100020204     C                   ENDIF
049200020204     C                   ENDIF
049300020204     C*
049400020204     C                   ENDSR
049500020204     C***
049600020204
049700010330
049800010330
049900010330     C*----------------------------------------------------*
050000010330     C*  CONTROLLO NUMERICITA' CAMPI
050100010330     C*----------------------------------------------------*
050200010330     C     CHKNUM        BEGSR
050300010330     C*
050400010606     C                   IF        PiDecChr = *blanks
050500010606     C                   EVAL      PiDecChr = ','
050600010606     C                   ENDIF
050700010606     C*
050800010606     C                   CALL(e)   'ISNUMERIC'
050900010330     C                   PARM                    PiStr            30
051000010606     C                   PARM                    PiDecChr          1
051100010330     C                   PARM      *ZEROS        PiVal            30 9
051200010330     C                   PARM      '0'           PiInt             1
051300010330     C                   PARM      '0'           PiNum             1
051400010330     C                   IF        %error
051500010606     C                   EVAL      PiNum=*off
051600010330     C                   ENDIF
051700010330     C*
051800010330     C                   ENDSR
051900010330     C***
052000010330
052100010601
052200010601
052300010601
052400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
052500010601     C     repfil        BEGSR
052600010601     C*
052700010601     C                   if        invfil = *zeros and
052800010601     C                             depfil > *zeros and
052900010629     C                             (vinflg = *blanks or
053000010629     C                              vinflg = *zeros)
053100010601     C
053200010601     C                   eval      invfil = depfil
053300010601     C                   endif
053400010601     C*
053500010601     C                   if        depfil <> invfil and
053600010601     C                             invfil > *zeros
053700010601     C                   eval      flgMulti = '1'
053800010601     C                   if        vinflg = *blanks
053900010601     C                   add       1             cntNonEl
054000010601     C                   endif
054100010601     C                   endif
054200010601     C*
054300010601     C                   if        vinflg = '2'
054400010601     C                   eval      flgStato = '2'
054500010601     C                   endif
054600010601     C*
054700010601     C                   ENDSR
054800010601     C***
054900010601
055000010601
055100010601
055200010330
055300010330
055400010330
055500990920      /TITLE Invio dei dati al punto operativo.
055600000613     C     invio         BEGSR
055700990920     C*
055800990920     C                   reset                   dscmz
055900010601     C                   move      invfil        cmzdst
056000990920     C                   eval      cmzfld = vlrfou
056100990920     C                   eval      cmzmbd = vlrhdl
056200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
056300000710     C                   if        prmfir = *blanks
056400021014     C                   eval      cmzfla = 'FIVAB00F'
056500021014     C                   eval      cmzmba = 'FIVAB00F'
056600000710     C                   else
056700000710     C                   eval      cmzfla = prmfir
056800000710     C                   eval      cmzmba = prmfir
056900000710     C                   endif
057000990920     C                   eval      cmznrr = *zeros
057100990920     C                   move      §ctrok        cmznrr
057200021018     C                   eval      cmzlba = vlrfl1
057300990920     C                   call(e)   'TIS711C'
057400990920     C                   parm                    dscmz
057500990921     C                   parm      *blanks       esito
057600990920     C                   if        %error
057700990920     C                             or cmzerr = '1'
057800990921     C                             or esito  = '1'
057900000710     C                   eval      wrkesito = '3'
058000990920     C                   endif
058100990920     C*
058200000613     C                   ENDSR
058300990910
058400010601
058500010601
058600010601
058700010601
058800010601      /TITLE Invio dei dati al punto operativo.
058900010601     C     opeini        BEGSR
059000010601     C*
059100010601     C* Inizializzo flag e contatori operativi
059200010601     C                   movel     '0'           flgGiro           1
059300010601     C                   movel     '0'           flgMulti          1
059400010601     C                   movel     '1'           flgStato          1
059500010615     C                   movel     '0'           flgOk             1
059600010601     C                   z-add     *zeros        cntNonEl         10 0
059700010601     C                   z-add     *zeros        depfil            3 0
059800010601     C                   z-add     *zeros        invfil            3 0
059900010601     C*
060000010601     C                   ENDSR
060100010601     C***
060200010601
060300010601
060400010601
060500010330
060600010330
060700000613     C     *inzsr        BEGSR
060800990910     C*
060900990910     C     *entry        plist
061000990920     C                   parm                    tivlrds
061100990921     C                   parm      wrkesito      esito
061200000724     C                   parm                    prmlit
061300000710     C                   parm                    prmfir
061400010330     C*
061500010330     C* CALCOLA LA DATA CORRENTE
061600010330     C                   time                    wn14             14 0
061700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061800010330     C                   z-add     wn8           g08dat
061900010330     C                   z-add     *zeros        g08inv
062000010330     C                   movel     '0'           g08err
062100010330     C                   call      'XSRDA8'
062200010330     C                   parm                    wlbda8
062300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062400000613     C*
062500000613     C                   ENDSR
062600000613     C***
062700990908
062800060901     Otitvu7p   E            riepilogo         2
062900990915     O                                              'Upload via Internet'
063000990915     O                                           +1 'Traduzione TIVIN00R -'
063100021014     O                                           +1 'FIVABWWR'
063200990915     O                                           +1 'Testate bolle'
063300990915     O          E            riepilogo   2
063400990915     O                       wrkdata
063500990915     O                       wrkora              +1
063600990915     O                       procname            +1
063700990915     O          E            riepilogo   2
063800990915     O                                              'Cliente..................:'
063900010605     O                       VABCCM        z     +1
064000990915     O          E            riepilogo   2
064100990920     O                                              'Riferimento Strategi.....:'
064200990920     O                       vlrhdl              +1
064300990915     O          E            riepilogo   2
064400990915     O                                              'Giusti...................:'
064500971022     O                       §CTROK        2   +  1
064600990915     O          E            riepilogo   2
064700990915     O                                              'Sbagliati e corretti.....:'
064800971022     O                       §CTRMO        2   +  1
064900990915     O          E            riepilogo   2
065000990915     O                                              'Sbagliati e scartati.....:'
065100971022     O                       §CTRNO        2   +  1
065200000613
065300060901     Otitvu7ps  E            testdett          2
065400000613     O                                              'Upload via Internet'
065500000613     O                                           +1 'Traduzione TIVIN00R -'
065600021014     O                                           +1 'FIVABWWR'
065700000613     O                                           +1 'Testate bolle'
065800000616     O          E            testdett    3
065900000613     O                                           +2 'N° rec'
066000000613     O                                           +3 'Anteprima contenuto'
066100000616     O          E            rigadett    2
066200000613     O                       rrnum               +2
066300000621     O                       recko               +3
066400000616     O          E            findett     2
066500000613     O                       wrkdata
066600000613     O                       wrkora              +1
066700000613     O                       procname            +1
066800000616     O          E            findett     2
066900000613     O                                              'Cliente..................:'
067000010605     O                       VABCCM        z     +1
067100000616     O          E            findett     2
067200000613     O                                              'Riferimento Strategi.....:'
067300000613     O                       vlrhdl              +1
067400000616     O          E            findett     2
067500000613     O                                              'Giusti...................:'
067600000613     O                       §CTROK        2   +  1
067700000616     O          E            findett     2
067800000613     O                                              'Sbagliati e corretti.....:'
067900000613     O                       §CTRMO        2   +  1
068000000616     O          E            findett     2
068100000613     O                                              'Sbagliati e scartati.....:'
068200000613     O                       §CTRNO        2   +  1
068300000613** CMD - COMANDI CL
068400060901OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVU7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
068500060901OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVU7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
068600060901DLTOVR FILE(TITVU7P TITVU7PS) LVL(*)
068700010606
068800000613
