000100050908      /TITLE Upload via Internet: traduzione in FIVABWWR. (x APRIMATIC  cod.2750832)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700050908     Ftitvo1p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900050908     Ftitvo1ps  O    f  198        PRINTER usropn
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
004000050518     D*------------------
004100050518     D* DS ARCHITETTURA
004200050518     D*------------------
004300050518     D kpjba         e ds                  inz
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
009700050908     C                   if        not %open(titvo1ps)
009800050908     C                   open      titvo1ps
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
010900010601     C*--------------------------------------------------------
011000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011100010601     C*--------------------------------------------------------
011200010601     C     ENDELA        BEGSR
011300990915     C*
011400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011500000613     C*
011600050908     C                   if        %open(titvo1ps)
011700000616     C                   except    findett
011800050908     C                   close     titvo1ps
011900000613     C                   endif
012000000616     C*
012100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012200011113     C                   Z-ADD     150           LENGH            15 5
012300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012400000616     C                   PARM                    CMD(3)
012500000616     C                   PARM                    LENGH
012600000616     C*
012700010601     C                   ENDSR
012800010601     C***
012900010601
013000010601
013100000613
013200000613     C*--------------------------------------------------------
013300000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013400000613     C*--------------------------------------------------------
013500000621     C     STPR          BEGSR
013600000613     C*
013700050908     C                   if        not %open(titvo1p)
013800050908     C                   open      titvo1p
013900990915     C                   endif
014000990915     C*
014100990915     C                   except    riepilogo
014200990915     C*
014300050908     C                   if        %open(titvo1p)
014400050908     C                   close     titvo1p
014500990914     C                   endif
014600000613     C*
014700000613     C                   ENDSR
014800000613     C***
014900000613
015000010601
015100010601
015200010330     C*--------------------------------------------------------
015300050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
015400010330     C*--------------------------------------------------------
015500010605     C     RWVAB         BEGSR
015600050408     C*
015700010330     C                   if        not %open(tivin00r)
015800010330     C                   open      tivin00r
015900010330     C                   endif
016000050408     C                   if        not %open(fivabwwr)
016100050408     C                   open      fivabwwr
016200010330     C                   endif
016300010330     C*
016400010604     C                   clear                   §CTROK
016500010604     C                   clear                   §CTRMO
016600010604     C                   clear                   §CTRNO
016700010330     C*
016800010330     C                   DO        *HIVAL
016900010330     C*
017000010330     C                   READ      tivin00r                               70
017100010618     C*
017200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017300010618     C                   if        vinflg = '1'
017400010618     C                   eval      flgOk = '1'
017500010618     C                   endif
017600010618     C*
017700010330     C                   if        vindta > *blanks
017800010330     C                   add       1             rrnum
017900010330     C*
018000010601     C                   if        *in70 = *off and
018100010330     C                             (vinflg = *blanks
018200010330     C                              or vinflg = '0'
018300010330     C                              or vinflg = '2')
018400010330     C*
018500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018600010711     C                   if        vinflg = *blanks or vinflg = '0'
018700010711     C                   clear                   vinmsg
018800010711     C                   endif
018900010601     C*
019000010330     C                   exsr      inzvar
019100010330     C                   exsr      defcam
019200010605     C                   exsr      impvab
019300010601     C*
019400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019500010605     C                   eval      depfil = VABLNP
019600010601     C                   exsr      repfil
019700010601     C                   if        depfil = invfil
019800021025     C                   if        vlrpoi = 999
019900021025     C                   MOVE(P)   invfil        VABFGS
020000021025     C                   else
020100021025     C                   MOVE(P)   vlrpoi        VABFGS
020200021025     C                   endif
020300010601     C*
020400010601     C                   exsr      PREELA
020500010601     C*
020600010604     C* Ebbene...
020700010604     C*
020800010604     C  N31              ADD       1             §CTROK            7 0
020900010604     C   32              ADD       1             §CTRMO            7 0
021000010604     C   31              ADD       1             §CTRNO            7 0
021100020205     C*
021200050408     C* Scarico il buffer del FIVAB
021300050408     C  N31              WRITE     FIVAB000
021400010604     C*
021500020722     C                   endif
021600020722     C*
021700010604     C                   if        *in31 = *off and
021800010604     C                             *in32 = *off
021900010604     C                   eval      vinflg = '1'
022000010604     C                   else
022100010604     C                   eval      recko = vindta
022200010604     C                   except    rigadett
022300010604     C                   eval      vinflg = '2'
022400010604     C                   endif
022500010604     C                   endif
022600010604     C*
022700010330     C                   else
022800010330     C                   eval      vinflg = '1'
022900010330     C                   endif
023000010601     C*
023100010601     C  N70              update    tivin000
023200010330     C*
023300010330     C  N70              ENDdo
023400010601     C*
023500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023600010601     C                   if        cntNonEl = *zeros or
023700010601     C                             flgMulti = '0'
023800010330     C* Se non ci sono record con errori ...
023900010601     C                   if        §ctrno = 0 and
024000010604     C                             §ctrmo = 0 and
024100010601     C                             flgStato <> '2'
024200010330     C* ... restituisco esito OK.
024300010330     C                   eval      wrkesito = '0'
024400010330     C                   else
024500010330     C                   if        §ctrok > 0
024600010330     C                   eval      wrkesito = '1'
024700010330     C                   else
024800010615     C                   if        flgOk = '0'
024900010615     C                   eval      wrkesito = '2'
025000010615     C                   else
025100010615     C                   eval      wrkesito = '6'
025200010615     C                   endif
025300010330     C                   endif
025400010330     C                   endif
025500010601     C                   else
025600010601     C                   eval      wrkesito = '9'
025700010601     C                   endif
025800010330     C*
025900010330     C                   if        %open(tivin00r)
026000010330     C                   close     tivin00r
026100010330     C                   endif
026200050408     C                   if        %open(fivabwwr)
026300050408     C                   close     fivabwwr
026400010330     C                   endif
026500010601     C*
026600010601     C                   if        vlrpoi <> 999
026700010601     C                   eval      invfil = vlrpoi
026800010601     C                   endif
026900010330     C*
027000010330     C                   if        §ctrok > 0
027100010601     C                             and invfil > *zeros
027200010330     C                   exsr      invio
027300010330     C                   endif
027400010601     C*
027500010618     C                   if        flgGiro = '1'
027600010601     C                   exsr      endela
027700010618     C                   endif
027800010330     C*
027900010330     C                   ENDSR
028000010330     C***
028100010330
028200010601
028300010601
028400010601
028500010601
028600010330     C*----------------------------------------------------*
028700020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028800010330     C*----------------------------------------------------*
028900010330     C     INZVAR        BEGSR
029000010330     C*
029100020204     C                   Z-ADD     *zeros        Num5_0            5 0
029200020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029300020322     C                   MOVEL     '0'           FlgCAS            1
029400010330     C*
029500010330     C                   ENDSR
029600010330     C*----------------------------------------------------*
029700020722     C*  IMPOSTAZIONE CAMPI COSTANTI
029800010330     C*----------------------------------------------------*
029900010330     C     DEFCAM        BEGSR
030000010330     C*
030100020204     C* Inizializzo il buffer del record da scrivere
030200050408     C                   CLEAR                   FIVAB000
030300020204     C* Imposto i valori di default...
030400050908     C                   EVAL      VABCCM = 2750832
030500050908     C                   EVAL      VABLNP = 275
030600050518     C                   EVAL      VABCTR = 000
030700050518     C                   EVAL      VABCBO = '1'
030800020204     C* ... e poi verifico se sono stati passati come parametri
030900020204     C                   IF        vlrppt > *blanks
031000040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031200020204     C                   EXSR      CHKNUM
031300020204     C                   IF        PiInt=*on
031400020204     C                   Z-ADD     PiVal         VABCCM
031500020204     C                   ENDIF
031600040301     C                   ENDIF
031700040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
031800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031900020204     C                   EXSR      CHKNUM
032000020204     C                   IF        PiInt=*on
032100020204     C                   Z-ADD     PiVal         VABLNP
032200020204     C                   ENDIF
032300040301     C                   ENDIF
032400040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
032500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032600020204     C                   EXSR      CHKNUM
032700020204     C                   IF        PiInt=*on
032800020204     C                   Z-ADD     PiVal         VABCTR
032900040301     C                   ENDIF
033000020204     C                   ENDIF
033100020204     C                   ENDIF
033200020204     C*
033300010330     C                   ENDSR
033400010607     C*----------------------------------------------------*
033500050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033600010607     C*----------------------------------------------------*
033700010607     C     IMPVAB        BEGSR
033800010607     C*
033900010607     C                   SETOFF                                       3132
034000010607     C*
034100010607     C* Reperimento campi ALFA
034200010607     C*
034300010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034400050908     C                   EVAL      VABRSD=%trim(%subst(vindta:50:35))
034500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034700020117     C* ==
034800050908     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
034900050908     C                   EVAL      VABLOD=%trim(%subst(vindta:120:25))
035000050908     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
035100060329     C                   EVAL      VABRMA=%trim(%subst(vindta:35:13))
035200050908     C                   EVAL      VABNOT=%trim(%subst(vindta:406:20))
035300010607     C*
035400010607     C* Reperimento campi NUMERICI
035500020722     C                   MOVEL     DATCOR        VABAAS
035600020722     C                   MOVE      DATCOR        VABMGS
035700050518     C* RMN
035800060329     C                   EVAL      PiStr=%trim(%subst(vindta:35:6))
035900050408     C                   EXSR      CHKNUM
036000050408     C                   IF        PiInt=*on
036100050408     C                   Z-ADD     PiVal         VABRMN
036200050908     C                   Z-ADD     PiVal         VABNSP
036300050408     C                   ELSE
036400050518     C                   SETON                                        32
036500050408     C                   Z-ADD     *zeros        VABRMN
036600050408     C                   EVAL      vinmsg = %trimr(vinmsg)
036700050908     C                             + ' ' + 'VABRMN VABNSP'
036800050408     C                   ENDIF
036900040420     C* CAD
037000050908     C                   EVAL      PiStr=%trim(%subst(vindta:145:9))
037100010607     C                   EXSR      CHKNUM
037200010607     C                   IF        PiInt=*on
037300020204     C                   Z-ADD     PiVal         Num5_0
037400020204     C                   MOVEL(p)  Num5_0        VABCAD
037500010607     C                   ELSE
037600010607     C                   SETON                                        32
037700020204     C                   EVAL      VABCAD = *zeros
037800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
037900020204     C                             + ' ' + 'VABCAD'
038000010607     C                   ENDIF
038100040420     C* NCL
038200050908     C                   EVAL      PiStr=%trim(%subst(vindta:164:5))
038300010607     C                   EXSR      CHKNUM
038400010607     C                   IF        PiInt=*on
038500010607     C                   Z-ADD     PiVal         VABNCL
038600010607     C                   ELSE
038700010607     C                   SETON                                        32
038800010607     C                   Z-ADD     *zeros        VABNCL
038900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039000010607     C                             + ' ' + 'VABNCL'
039100010607     C                   ENDIF
039200040420     C* PKB
039300050908     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
039400010607     C                   EXSR      CHKNUM
039500010607     C                   IF        PiNum=*on
039600050330     C                   Z-ADD(H)  PiVal         VABPKB
039700010607     C                   ELSE
039800010607     C                   SETON                                        32
039900010607     C                   Z-ADD     *zeros        VABPKB
040000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040100010607     C                             + ' ' + 'VABPKB'
040200010607     C                   ENDIF
040300050908     C* CAS
040400060406     C                   IF        %trim(%subst(vindta:196:14)) <> *zeros
040500050908     C                   EVAL      FlgCAS = '1'
040600050908     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
040700050908     C                   EXSR      CHKNUM
040800050908     C                   IF        PiNum=*on
040900050908     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
041000050908     C                   Z-ADD     PiVal         VABCAS
041100050908     C                   EVAL      VABVCA = 'EUR'
041200050908     C                   ELSE
041300050908     C                   SETON                                        32
041400050908     C                   Z-ADD     *zeros        VABCAS
041500050908     C                   EVAL      vinmsg = %trimr(vinmsg)
041600050908     C                             + ' ' + 'VABCAS'
041700050908     C                   ENDIF
041800050908     C                   ENDIF
041900050908     C* DCR
042000050908     C                   IF        %subst(vindta:11:8) <> *zeros
042100050908     C                   EVAL      PiStr=%trim(%subst(vindta:11:8))
042200050901     C                   EXSR      CHKNUM
042300050901     C                   IF        PiInt=*on
042400050901     C                   Z-ADD     PiVal         VABDCR
042500050901     C                   ELSE
042600050901     C                   SETON                                        32
042700050901     C                   Z-ADD     *zeros        VABDCR
042800050901     C                   EVAL      vinmsg = %trimr(vinmsg)
042900050901     C                             + ' ' + 'VABDCR'
043000050901     C                   ENDIF
043100050907     C                   ENDIF
043200050901     C* HCR
043300050908     C                   IF        %subst(vindta:19:4) <> *zeros
043400050908     C                   EVAL      PiStr=%trim(%subst(vindta:19:4))
043500050901     C                   EXSR      CHKNUM
043600050901     C                   IF        PiInt=*on
043700050901     C                   Z-ADD     PiVal         VABHCR
043800050901     C                   ELSE
043900050901     C                   SETON                                        32
044000050901     C                   Z-ADD     *zeros        VABHCR
044100050901     C                   EVAL      vinmsg = %trimr(vinmsg)
044200050901     C                             + ' ' + 'VABHCR'
044300050901     C                   ENDIF
044400050907     C                   ENDIF
044500020322     C*
044600020322     C* Considerazioni finali su CBO/CAS
044700050908     C                   IF        FlgCAS = '1'
044800040713     C                   IF        VABCBO = '1'
044900020322     C                   EVAL      VABCBO = '4'
045000040713     C                   ENDIF
045100040713     C                   IF        VABCBO = '2'
045200040713     C                   EVAL      VABCBO = '6'
045300040713     C                   ENDIF
045400020322     C                   ENDIF
045500020204     C*
045600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045700020204     C                   EXSR      CHKIMPDIV
045800020204     C*
045900010607     C                   ENDSR
046000010607     C*----------------------------------------------------*
046100010601
046200020204
046300020204     C*----------------------------------------------------*
046400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046500020204     C*----------------------------------------------------*
046600020204     C     CHKIMPDIV     BEGSR
046700020204     C*
046800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046900020204     C                   Z-ADD     *zeros        wrkDec            9 9
047000020204     C*
047100020204     C* Come prima cosa effettuo considerazioni sulla divisa
047200020204     C                   IF        vabIAS > *zeros
047300020204     C                   IF        vabVAS <> 'EUR'
047400020204     C                   EVAL      vabVAS =  'ITL'
047500020204     C                   ENDIF
047600020204     C                   ENDIF
047700020204     C*
047800020204     C                   IF        vabCAS > *zeros
047900020204     C                   IF        vabVCA <> 'EUR'
048000020204     C                   EVAL      vabVCA =  'ITL'
048100020204     C                   ENDIF
048200020204     C                   ENDIF
048300020204     C*
048400020204     C                   IF        vabVMD > *zeros
048500020321     C                   IF        vabVAD <> 'EUR'
048600020204     C                   EVAL      vabVAD =  'ITL'
048700020204     C                   ENDIF
048800020204     C                   ENDIF
048900020204     C*
049000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049100020204     C                   Z-ADD     vabIAS        wrkDec
049200020204     C                   IF        wrkDec > *zeros
049300020204     C                   IF        vabVAS = 'ITL'
049400020204     C                   EVAL      vabIAS = *zeros
049500020204     C                   ENDIF
049600020204     C                   ENDIF
049700020204     C*
049800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
049900020204     C                   Z-ADD     vabCAS        wrkDec
050000020204     C                   IF        wrkDec > *zeros
050100020204     C                   IF        vabVCA = 'ITL'
050200020204     C                   EVAL      vabCAS = *zeros
050300020204     C                   ENDIF
050400020204     C                   ENDIF
050500020204     C*
050600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050700020204     C                   Z-ADD     vabVMD        wrkDec
050800020204     C                   IF        wrkDec > *zeros
050900020204     C                   IF        vabVAD = 'ITL'
051000020204     C                   EVAL      vabVMD = *zeros
051100020204     C                   ENDIF
051200020204     C                   ENDIF
051300020204     C*
051400020204     C                   ENDSR
051500020204     C***
051600020204
051700010330
051800010330
051900010330     C*----------------------------------------------------*
052000010330     C*  CONTROLLO NUMERICITA' CAMPI
052100010330     C*----------------------------------------------------*
052200010330     C     CHKNUM        BEGSR
052300010330     C*
052400010606     C                   IF        PiDecChr = *blanks
052500010606     C                   EVAL      PiDecChr = ','
052600010606     C                   ENDIF
052700010606     C*
052800010606     C                   CALL(e)   'ISNUMERIC'
052900010330     C                   PARM                    PiStr            30
053000010606     C                   PARM                    PiDecChr          1
053100010330     C                   PARM      *ZEROS        PiVal            30 9
053200010330     C                   PARM      '0'           PiInt             1
053300010330     C                   PARM      '0'           PiNum             1
053400010330     C                   IF        %error
053500010606     C                   EVAL      PiNum=*off
053600010330     C                   ENDIF
053700010330     C*
053800010330     C                   ENDSR
053900010330     C***
054000010330
054100010601
054200010601
054300010601
054400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
054500010601     C     repfil        BEGSR
054600010601     C*
054700010601     C                   if        invfil = *zeros and
054800010601     C                             depfil > *zeros and
054900010629     C                             (vinflg = *blanks or
055000010629     C                              vinflg = *zeros)
055100010601     C
055200010601     C                   eval      invfil = depfil
055300010601     C                   endif
055400010601     C*
055500010601     C                   if        depfil <> invfil and
055600010601     C                             invfil > *zeros
055700010601     C                   eval      flgMulti = '1'
055800010601     C                   if        vinflg = *blanks
055900010601     C                   add       1             cntNonEl
056000010601     C                   endif
056100010601     C                   endif
056200010601     C*
056300010601     C                   if        vinflg = '2'
056400010601     C                   eval      flgStato = '2'
056500010601     C                   endif
056600010601     C*
056700010601     C                   ENDSR
056800010601     C***
056900010601
057000010601
057100010601
057200010330
057300010330
057400010330
057500990920      /TITLE Invio dei dati al punto operativo.
057600000613     C     invio         BEGSR
057700990920     C*
057800990920     C                   reset                   dscmz
057900010601     C                   move      invfil        cmzdst
058000990920     C                   eval      cmzfld = vlrfou
058100990920     C                   eval      cmzmbd = vlrhdl
058200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
058300000710     C                   if        prmfir = *blanks
058400050408     C                   eval      cmzfla = 'FIVAB00F'
058500050408     C                   eval      cmzmba = 'FIVAB00F'
058600000710     C                   else
058700000710     C                   eval      cmzfla = prmfir
058800000710     C                   eval      cmzmba = prmfir
058900000710     C                   endif
059000990920     C                   eval      cmznrr = *zeros
059100990920     C                   move      §ctrok        cmznrr
059200021018     C                   eval      cmzlba = vlrfl1
059300990920     C                   call(e)   'TIS711C'
059400990920     C                   parm                    dscmz
059500990921     C                   parm      *blanks       esito
059600990920     C                   if        %error
059700990920     C                             or cmzerr = '1'
059800990921     C                             or esito  = '1'
059900000710     C                   eval      wrkesito = '3'
060000990920     C                   endif
060100990920     C*
060200000613     C                   ENDSR
060300990910
060400010601
060500010601
060600010601
060700010601
060800010601      /TITLE Invio dei dati al punto operativo.
060900010601     C     opeini        BEGSR
061000010601     C*
061100010601     C* Inizializzo flag e contatori operativi
061200010601     C                   movel     '0'           flgGiro           1
061300010601     C                   movel     '0'           flgMulti          1
061400010601     C                   movel     '1'           flgStato          1
061500010615     C                   movel     '0'           flgOk             1
061600010601     C                   z-add     *zeros        cntNonEl         10 0
061700010601     C                   z-add     *zeros        depfil            3 0
061800010601     C                   z-add     *zeros        invfil            3 0
061900010601     C*
062000010601     C                   ENDSR
062100010601     C***
062200010601
062300010601
062400010601
062500010330
062600010330
062700000613     C     *inzsr        BEGSR
062800990910     C*
062900990910     C     *entry        plist
063000990920     C                   parm                    tivlrds
063100990921     C                   parm      wrkesito      esito
063200000724     C                   parm                    prmlit
063300000710     C                   parm                    prmfir
063400010330     C*
063500010330     C* CALCOLA LA DATA CORRENTE
063600010330     C                   time                    wn14             14 0
063700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063800010330     C                   z-add     wn8           g08dat
063900010330     C                   z-add     *zeros        g08inv
064000010330     C                   movel     '0'           g08err
064100010330     C                   call      'XSRDA8'
064200010330     C                   parm                    wlbda8
064300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
064400000613     C*
064500000613     C                   ENDSR
064600000613     C***
064700990908
064800050908     Otitvo1p   E            riepilogo         2
064900990915     O                                              'Upload via Internet'
065000990915     O                                           +1 'Traduzione TIVIN00R -'
065100050408     O                                           +1 'FIVABWWR'
065200990915     O                                           +1 'Testate bolle'
065300990915     O          E            riepilogo   2
065400990915     O                       wrkdata
065500990915     O                       wrkora              +1
065600990915     O                       procname            +1
065700990915     O          E            riepilogo   2
065800990915     O                                              'Cliente..................:'
065900010605     O                       VABCCM        z     +1
066000990915     O          E            riepilogo   2
066100990920     O                                              'Riferimento Strategi.....:'
066200990920     O                       vlrhdl              +1
066300990915     O          E            riepilogo   2
066400990915     O                                              'Giusti...................:'
066500971022     O                       §CTROK        2   +  1
066600990915     O          E            riepilogo   2
066700990915     O                                              'Sbagliati e corretti.....:'
066800971022     O                       §CTRMO        2   +  1
066900990915     O          E            riepilogo   2
067000990915     O                                              'Sbagliati e scartati.....:'
067100971022     O                       §CTRNO        2   +  1
067200000613
067300050908     Otitvo1ps  E            testdett          2
067400000613     O                                              'Upload via Internet'
067500000613     O                                           +1 'Traduzione TIVIN00R -'
067600050408     O                                           +1 'FIVABWWR'
067700000613     O                                           +1 'Testate bolle'
067800000616     O          E            testdett    3
067900000613     O                                           +2 'N° rec'
068000000613     O                                           +3 'Anteprima contenuto'
068100000616     O          E            rigadett    2
068200000613     O                       rrnum               +2
068300000621     O                       recko               +3
068400000616     O          E            findett     2
068500000613     O                       wrkdata
068600000613     O                       wrkora              +1
068700000613     O                       procname            +1
068800000616     O          E            findett     2
068900000613     O                                              'Cliente..................:'
069000010605     O                       VABCCM        z     +1
069100000616     O          E            findett     2
069200000613     O                                              'Riferimento Strategi.....:'
069300000613     O                       vlrhdl              +1
069400000616     O          E            findett     2
069500000613     O                                              'Giusti...................:'
069600000613     O                       §CTROK        2   +  1
069700000616     O          E            findett     2
069800000613     O                                              'Sbagliati e corretti.....:'
069900000613     O                       §CTRMO        2   +  1
070000000616     O          E            findett     2
070100000613     O                                              'Sbagliati e scartati.....:'
070200000613     O                       §CTRNO        2   +  1
070300000613** CMD - COMANDI CL
070400050908OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVO1P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
070500050908OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVO1PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
070600050908DLTOVR FILE(TITVO1P TITVO1PS) LVL(*)
070700010606
070800000613
