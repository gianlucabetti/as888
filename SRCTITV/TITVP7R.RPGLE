000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700051219     Ftitvp7p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900051219     Ftitvp7ps  O    f  198        PRINTER usropn
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
004000010330
004100010330
004200990915     C                   time                    wrkdata
004300990915     C                   time                    wrkora
004400000913     C                   reset                   rrnum
004500990921     C                   reset                   esito
004600990921     C                   reset                   wrkesito
004700010601     C*
004800010601     C                   exsr      opeini
004900010605     C                   exsr      rwvab
005000010601     C*
005100010601     C                   seton                                        lr
005200010601
005300010601
005400010601
005500010601
005600010601     C*--------------------------------------------------------
005700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005800010601     C*--------------------------------------------------------
005900010601     C     PREELA        BEGSR
006000010601     C*
006100010601     C* SE OCCORRE SPEDIRE IN FILIALE
006200010601     C                   if        invfil <> *zeros and
006300010601     C                             flgGiro = '0'
006400010601     C*
006500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006600010601     C                   eval      flgGiro = '1'
006700000724     C*
006800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007000010601     C     invfil        chain     azorg01l
007100000724     C                   if        %found
007200000724     C                   movel(p)  CMD(1)        depcmd
007300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007400000724     C*
007500000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007600011113     C                   Z-ADD     150           LENGH            15 5
007700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007800000724     C                   PARM                    depcmd
007900000724     C                   PARM                    LENGH
008000000724     C*
008100000724     C                   endif
008200000616     C*
008300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008400000616     C                   movel(p)  CMD(2)        depcmd
008500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008600000616     C*
008700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008800011113     C                   Z-ADD     150           LENGH            15 5
008900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009000000616     C                   PARM                    depcmd
009100000616     C                   PARM                    LENGH
009200000613     C*
009300051219     C                   if        not %open(titvp7ps)
009400051219     C                   open      titvp7ps
009500000616     C                   except    testdett
009600000613     C                   endif
009700010601     C*
009800010601     C                   endif
009900010601     C*
010000010601     C                   ENDSR
010100010601     C***
010200010601
010300010601
010400010601
010500010601     C*--------------------------------------------------------
010600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010700010601     C*--------------------------------------------------------
010800010601     C     ENDELA        BEGSR
010900990915     C*
011000000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011100000613     C*
011200051219     C                   if        %open(titvp7ps)
011300000616     C                   except    findett
011400051219     C                   close     titvp7ps
011500000613     C                   endif
011600000616     C*
011700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011800011113     C                   Z-ADD     150           LENGH            15 5
011900010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012000000616     C                   PARM                    CMD(3)
012100000616     C                   PARM                    LENGH
012200000616     C*
012300010601     C                   ENDSR
012400010601     C***
012500010601
012600010601
012700000613
012800000613     C*--------------------------------------------------------
012900000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013000000613     C*--------------------------------------------------------
013100000621     C     STPR          BEGSR
013200000613     C*
013300051219     C                   if        not %open(titvp7p)
013400051219     C                   open      titvp7p
013500990915     C                   endif
013600990915     C*
013700990915     C                   except    riepilogo
013800990915     C*
013900051219     C                   if        %open(titvp7p)
014000051219     C                   close     titvp7p
014100990914     C                   endif
014200000613     C*
014300000613     C                   ENDSR
014400000613     C***
014500000613
014600010601
014700010601
014800010330     C*--------------------------------------------------------
014900050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
015000010330     C*--------------------------------------------------------
015100010605     C     RWVAB         BEGSR
015200050408     C*
015300010330     C                   if        not %open(tivin00r)
015400010330     C                   open      tivin00r
015500010330     C                   endif
015600050408     C                   if        not %open(fivabwwr)
015700050408     C                   open      fivabwwr
015800010330     C                   endif
015900010330     C*
016000010604     C                   clear                   §CTROK
016100010604     C                   clear                   §CTRMO
016200010604     C                   clear                   §CTRNO
016300010330     C*
016400010330     C                   DO        *HIVAL
016500010330     C*
016600010330     C                   READ      tivin00r                               70
016700010618     C*
016800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016900010618     C                   if        vinflg = '1'
017000010618     C                   eval      flgOk = '1'
017100010618     C                   endif
017200010618     C*
017300010330     C                   if        vindta > *blanks
017400010330     C                   add       1             rrnum
017500010330     C*
017600010601     C                   if        *in70 = *off and
017700010330     C                             (vinflg = *blanks
017800010330     C                              or vinflg = '0'
017900010330     C                              or vinflg = '2')
018000010330     C*
018100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018200010711     C                   if        vinflg = *blanks or vinflg = '0'
018300010711     C                   clear                   vinmsg
018400010711     C                   endif
018500010601     C*
018600010330     C                   exsr      inzvar
018700010330     C                   exsr      defcam
018800010605     C                   exsr      impvab
018900010601     C*
019000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019100010605     C                   eval      depfil = VABLNP
019200010601     C                   exsr      repfil
019300010601     C                   if        depfil = invfil
019400021025     C                   if        vlrpoi = 999
019500021025     C                   MOVE(P)   invfil        VABFGS
019600021025     C                   else
019700021025     C                   MOVE(P)   vlrpoi        VABFGS
019800021025     C                   endif
019900010601     C*
020000010601     C                   exsr      PREELA
020100010601     C*
020200010604     C* Ebbene...
020300010604     C*
020400010604     C  N31              ADD       1             §CTROK            7 0
020500010604     C   32              ADD       1             §CTRMO            7 0
020600010604     C   31              ADD       1             §CTRNO            7 0
020700020205     C*
020800050408     C* Scarico il buffer del FIVAB
020900050408     C  N31              WRITE     FIVAB000
021000010604     C*
021100020722     C                   endif
021200020722     C*
021300010604     C                   if        *in31 = *off and
021400010604     C                             *in32 = *off
021500010604     C                   eval      vinflg = '1'
021600010604     C                   else
021700010604     C                   eval      recko = vindta
021800010604     C                   except    rigadett
021900010604     C                   eval      vinflg = '2'
022000010604     C                   endif
022100010604     C                   endif
022200010604     C*
022300010330     C                   else
022400010330     C                   eval      vinflg = '1'
022500010330     C                   endif
022600010601     C*
022700010601     C  N70              update    tivin000
022800010330     C*
022900010330     C  N70              ENDdo
023000010601     C*
023100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023200010601     C                   if        cntNonEl = *zeros or
023300010601     C                             flgMulti = '0'
023400010330     C* Se non ci sono record con errori ...
023500010601     C                   if        §ctrno = 0 and
023600010604     C                             §ctrmo = 0 and
023700010601     C                             flgStato <> '2'
023800010330     C* ... restituisco esito OK.
023900010330     C                   eval      wrkesito = '0'
024000010330     C                   else
024100010330     C                   if        §ctrok > 0
024200010330     C                   eval      wrkesito = '1'
024300010330     C                   else
024400010615     C                   if        flgOk = '0'
024500010615     C                   eval      wrkesito = '2'
024600010615     C                   else
024700010615     C                   eval      wrkesito = '6'
024800010615     C                   endif
024900010330     C                   endif
025000010330     C                   endif
025100010601     C                   else
025200010601     C                   eval      wrkesito = '9'
025300010601     C                   endif
025400010330     C*
025500010330     C                   if        %open(tivin00r)
025600010330     C                   close     tivin00r
025700010330     C                   endif
025800050408     C                   if        %open(fivabwwr)
025900050408     C                   close     fivabwwr
026000010330     C                   endif
026100010601     C*
026200010601     C                   if        vlrpoi <> 999
026300010601     C                   eval      invfil = vlrpoi
026400010601     C                   endif
026500010330     C*
026600010330     C                   if        §ctrok > 0
026700010601     C                             and invfil > *zeros
026800010330     C                   exsr      invio
026900010330     C                   endif
027000010601     C*
027100010618     C                   if        flgGiro = '1'
027200010601     C                   exsr      endela
027300010618     C                   endif
027400010330     C*
027500010330     C                   ENDSR
027600010330     C***
027700010330
027800010601
027900010601
028000010601
028100010601
028200010330     C*----------------------------------------------------*
028300020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028400010330     C*----------------------------------------------------*
028500010330     C     INZVAR        BEGSR
028600010330     C*
028700020204     C                   Z-ADD     *zeros        Num5_0            5 0
028800020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028900020322     C                   MOVEL     '0'           FlgCAS            1
029000010330     C*
029100010330     C                   ENDSR
029200010330     C*----------------------------------------------------*
029300020722     C*  IMPOSTAZIONE CAMPI COSTANTI
029400010330     C*----------------------------------------------------*
029500010330     C     DEFCAM        BEGSR
029600010330     C*
029700020204     C* Inizializzo il buffer del record da scrivere
029800050408     C                   CLEAR                   FIVAB000
029900020204     C* Imposto i valori di default...
030000051219     C                   EVAL      VABCCM = 0063058
030100051219     C                   EVAL      VABLNP = 006
030200051219     C                   EVAL      VABCTR = 500
030300050921     C                   EVAL      VABCBO = '1'
030400051219     C                   EVAL      VABCTM = '7Q'
030500020204     C* ... e poi verifico se sono stati passati come parametri
030600020204     C                   IF        vlrppt > *blanks
030700040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
030800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030900020204     C                   EXSR      CHKNUM
031000020204     C                   IF        PiInt=*on
031100020204     C                   Z-ADD     PiVal         VABCCM
031200020204     C                   ENDIF
031300040301     C                   ENDIF
031400040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
031500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031600020204     C                   EXSR      CHKNUM
031700020204     C                   IF        PiInt=*on
031800020204     C                   Z-ADD     PiVal         VABLNP
031900020204     C                   ENDIF
032000040301     C                   ENDIF
032100040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
032200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032300020204     C                   EXSR      CHKNUM
032400020204     C                   IF        PiInt=*on
032500020204     C                   Z-ADD     PiVal         VABCTR
032600040301     C                   ENDIF
032700020204     C                   ENDIF
032800020204     C                   ENDIF
032900020204     C*
033000010330     C                   ENDSR
033100010607     C*----------------------------------------------------*
033200050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033300010607     C*----------------------------------------------------*
033400010607     C     IMPVAB        BEGSR
033500010607     C*
033600010607     C                   SETOFF                                       3132
033700010607     C*
033800010607     C* Reperimento campi ALFA
033900010607     C*
034000010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034100051219     C                   EVAL      VABRSD=%trim(%subst(vindta:26:35))
034200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034400020117     C* ==
034500051219     C                   EVAL      VABIND=%trim(%subst(vindta:61:35))
034600051219     C                   EVAL      VABLOD=%trim(%subst(vindta:96:35))
034700051219     C                   EVAL      VABPRD=%trim(%subst(vindta:137:2))
034800051219     C                   EVAL      VABRMA=%trim(%subst(vindta:1:15))
034900051219     C                   EVAL      VABNOT=%trim(%subst(vindta:184:35))
035000051219     C                   EVAL      VABNT2=%trim(%subst(vindta:184+35:60-35))
035100051219     C                   MOVEL     *blanks       wDCR_Alfa         8
035200051219     C                   EVAL      wDCR_Alfa = %subst(vindta:249:4)+
035300051219     C                                         %subst(vindta:254:2)+
035400051219     C                                         %subst(vindta:257:2)
035500051219     C                   MOVE(P)   wDCR_Alfa     VABDCR
035600010607     C*
035700010607     C* Reperimento campi NUMERICI
035800020722     C                   MOVEL     DATCOR        VABAAS
035900020722     C                   MOVE      DATCOR        VABMGS
036000050518     C* RMN
036100051219     C                   EVAL      PiStr=%trim(%subst(vindta:1:15))
036200050408     C                   EXSR      CHKNUM
036300050408     C                   IF        PiInt=*on
036400050408     C                   Z-ADD     PiVal         VABRMN
036500050908     C                   Z-ADD     PiVal         VABNSP
036600050408     C                   ELSE
036700051219     C                   SETON                                        31
036800050408     C                   Z-ADD     *zeros        VABRMN
036900050408     C                   EVAL      vinmsg = %trimr(vinmsg)
037000050908     C                             + ' ' + 'VABRMN VABNSP'
037100050408     C                   ENDIF
037200040420     C* CAD
037300051219     C                   EVAL      PiStr=%trim(%subst(vindta:132:5))
037400010607     C                   EXSR      CHKNUM
037500010607     C                   IF        PiInt=*on
037600020204     C                   Z-ADD     PiVal         Num5_0
037700020204     C                   MOVEL(p)  Num5_0        VABCAD
037800010607     C                   ELSE
037900010607     C                   SETON                                        32
038000020204     C                   EVAL      VABCAD = *zeros
038100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038200020204     C                             + ' ' + 'VABCAD'
038300010607     C                   ENDIF
038400040420     C* NCL
038500051219     C                   EVAL      PiStr=%trim(%subst(vindta:140:5))
038600010607     C                   EXSR      CHKNUM
038700010607     C                   IF        PiInt=*on
038800010607     C                   Z-ADD     PiVal         VABNCL
038900010607     C                   ELSE
039000010607     C                   SETON                                        32
039100010607     C                   Z-ADD     *zeros        VABNCL
039200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039300010607     C                             + ' ' + 'VABNCL'
039400010607     C                   ENDIF
039500040420     C* PKB
039600051219     C                   EVAL      PiStr=%trim(%subst(vindta:147:7))
039700010607     C                   EXSR      CHKNUM
039800010607     C                   IF        PiNum=*on
039900050330     C                   Z-ADD(H)  PiVal         VABPKB
040000010607     C                   ELSE
040100010607     C                   SETON                                        32
040200010607     C                   Z-ADD     *zeros        VABPKB
040300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040400010607     C                             + ' ' + 'VABPKB'
040500010607     C                   ENDIF
040600051219     C* VLB
040700051219     C                   EVAL      PiStr=%trim(%subst(vindta:156:7))
040800051219     C                   EXSR      CHKNUM
040900051219     C                   IF        PiNum=*on
041000051219     C                   Z-ADD(H)  PiVal         VABVLB
041100051219     C                   Z-ADD(H)  PiVal         VABQFT
041200051219     C                   ELSE
041300051219     C                   SETON                                        32
041400051219     C                   Z-ADD     *zeros        VABVLB
041500051219     C                   Z-ADD     *zeros        VABQFT
041600051219     C                   EVAL      vinmsg = %trimr(vinmsg)
041700051219     C                             + ' ' + 'VABPKB VABQFT'
041800051219     C                   ENDIF
041900051219     C* CAS
042000051219     C                   IF        %trim(%subst(vindta:165:15))<>',000'
042100051219     C                   EVAL      PiStr=%trim(%subst(vindta:165:15))
042200051219     C                   EVAL      VABVCA=%trim(%subst(vindta:180:4))
042300051219     C                   EVAL      FlgCAS = '1'
042400051219     C                   EXSR      CHKNUM
042500051219     C                   IF        PiNum=*on
042600051219     C                   EVAL      VABCAS = PiVal
042700051219     C                   ELSE
042800051219     C                   SETON                                        32
042900051219     C                   Z-ADD     *zeros        VABCAS
043000051219     C                   EVAL      vinmsg = %trimr(vinmsg)
043100051219     C                             + ' ' + 'VABCAS'
043200051219     C                   ENDIF
043300051219     C                   ENDIF
043400020322     C*
043500020322     C* Considerazioni finali su CBO/CAS
043600050908     C                   IF        FlgCAS = '1'
043700040713     C                   IF        VABCBO = '1'
043800020322     C                   EVAL      VABCBO = '4'
043900040713     C                   ENDIF
044000040713     C                   IF        VABCBO = '2'
044100040713     C                   EVAL      VABCBO = '6'
044200040713     C                   ENDIF
044300020322     C                   ENDIF
044400020204     C*
044500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044600020204     C                   EXSR      CHKIMPDIV
044700020204     C*
044800010607     C                   ENDSR
044900010607     C*----------------------------------------------------*
045000010601
045100020204
045200020204     C*----------------------------------------------------*
045300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
045400020204     C*----------------------------------------------------*
045500020204     C     CHKIMPDIV     BEGSR
045600020204     C*
045700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
045800020204     C                   Z-ADD     *zeros        wrkDec            9 9
045900020204     C*
046000020204     C* Come prima cosa effettuo considerazioni sulla divisa
046100020204     C                   IF        vabIAS > *zeros
046200020204     C                   IF        vabVAS <> 'EUR'
046300020204     C                   EVAL      vabVAS =  'ITL'
046400020204     C                   ENDIF
046500020204     C                   ENDIF
046600020204     C*
046700020204     C                   IF        vabCAS > *zeros
046800020204     C                   IF        vabVCA <> 'EUR'
046900020204     C                   EVAL      vabVCA =  'ITL'
047000020204     C                   ENDIF
047100020204     C                   ENDIF
047200020204     C*
047300020204     C                   IF        vabVMD > *zeros
047400020321     C                   IF        vabVAD <> 'EUR'
047500020204     C                   EVAL      vabVAD =  'ITL'
047600020204     C                   ENDIF
047700020204     C                   ENDIF
047800020204     C*
047900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048000020204     C                   Z-ADD     vabIAS        wrkDec
048100020204     C                   IF        wrkDec > *zeros
048200020204     C                   IF        vabVAS = 'ITL'
048300020204     C                   EVAL      vabIAS = *zeros
048400020204     C                   ENDIF
048500020204     C                   ENDIF
048600020204     C*
048700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
048800020204     C                   Z-ADD     vabCAS        wrkDec
048900020204     C                   IF        wrkDec > *zeros
049000020204     C                   IF        vabVCA = 'ITL'
049100020204     C                   EVAL      vabCAS = *zeros
049200020204     C                   ENDIF
049300020204     C                   ENDIF
049400020204     C*
049500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
049600020204     C                   Z-ADD     vabVMD        wrkDec
049700020204     C                   IF        wrkDec > *zeros
049800020204     C                   IF        vabVAD = 'ITL'
049900020204     C                   EVAL      vabVMD = *zeros
050000020204     C                   ENDIF
050100020204     C                   ENDIF
050200020204     C*
050300020204     C                   ENDSR
050400020204     C***
050500020204
050600010330
050700010330
050800010330     C*----------------------------------------------------*
050900010330     C*  CONTROLLO NUMERICITA' CAMPI
051000010330     C*----------------------------------------------------*
051100010330     C     CHKNUM        BEGSR
051200010330     C*
051300010606     C                   IF        PiDecChr = *blanks
051400010606     C                   EVAL      PiDecChr = ','
051500010606     C                   ENDIF
051600010606     C*
051700010606     C                   CALL(e)   'ISNUMERIC'
051800010330     C                   PARM                    PiStr            30
051900010606     C                   PARM                    PiDecChr          1
052000010330     C                   PARM      *ZEROS        PiVal            30 9
052100010330     C                   PARM      '0'           PiInt             1
052200010330     C                   PARM      '0'           PiNum             1
052300010330     C                   IF        %error
052400010606     C                   EVAL      PiNum=*off
052500010330     C                   ENDIF
052600010330     C*
052700010330     C                   ENDSR
052800010330     C***
052900010330
053000010601
053100010601
053200010601
053300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
053400010601     C     repfil        BEGSR
053500010601     C*
053600010601     C                   if        invfil = *zeros and
053700010601     C                             depfil > *zeros and
053800010629     C                             (vinflg = *blanks or
053900010629     C                              vinflg = *zeros)
054000010601     C
054100010601     C                   eval      invfil = depfil
054200010601     C                   endif
054300010601     C*
054400010601     C                   if        depfil <> invfil and
054500010601     C                             invfil > *zeros
054600010601     C                   eval      flgMulti = '1'
054700010601     C                   if        vinflg = *blanks
054800010601     C                   add       1             cntNonEl
054900010601     C                   endif
055000010601     C                   endif
055100010601     C*
055200010601     C                   if        vinflg = '2'
055300010601     C                   eval      flgStato = '2'
055400010601     C                   endif
055500010601     C*
055600010601     C                   ENDSR
055700010601     C***
055800010601
055900010601
056000010601
056100010330
056200010330
056300010330
056400990920      /TITLE Invio dei dati al punto operativo.
056500000613     C     invio         BEGSR
056600990920     C*
056700990920     C                   reset                   dscmz
056800010601     C                   move      invfil        cmzdst
056900990920     C                   eval      cmzfld = vlrfou
057000990920     C                   eval      cmzmbd = vlrhdl
057100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
057200000710     C                   if        prmfir = *blanks
057300050408     C                   eval      cmzfla = 'FIVAB00F'
057400050408     C                   eval      cmzmba = 'FIVAB00F'
057500000710     C                   else
057600000710     C                   eval      cmzfla = prmfir
057700000710     C                   eval      cmzmba = prmfir
057800000710     C                   endif
057900990920     C                   eval      cmznrr = *zeros
058000990920     C                   move      §ctrok        cmznrr
058100021018     C                   eval      cmzlba = vlrfl1
058200990920     C                   call(e)   'TIS711C'
058300990920     C                   parm                    dscmz
058400990921     C                   parm      *blanks       esito
058500990920     C                   if        %error
058600990920     C                             or cmzerr = '1'
058700990921     C                             or esito  = '1'
058800000710     C                   eval      wrkesito = '3'
058900990920     C                   endif
059000990920     C*
059100000613     C                   ENDSR
059200990910
059300010601
059400010601
059500010601
059600010601
059700010601      /TITLE Invio dei dati al punto operativo.
059800010601     C     opeini        BEGSR
059900010601     C*
060000010601     C* Inizializzo flag e contatori operativi
060100010601     C                   movel     '0'           flgGiro           1
060200010601     C                   movel     '0'           flgMulti          1
060300010601     C                   movel     '1'           flgStato          1
060400010615     C                   movel     '0'           flgOk             1
060500010601     C                   z-add     *zeros        cntNonEl         10 0
060600010601     C                   z-add     *zeros        depfil            3 0
060700010601     C                   z-add     *zeros        invfil            3 0
060800010601     C*
060900010601     C                   ENDSR
061000010601     C***
061100010601
061200010601
061300010601
061400010330
061500010330
061600000613     C     *inzsr        BEGSR
061700990910     C*
061800990910     C     *entry        plist
061900990920     C                   parm                    tivlrds
062000990921     C                   parm      wrkesito      esito
062100000724     C                   parm                    prmlit
062200000710     C                   parm                    prmfir
062300010330     C*
062400010330     C* CALCOLA LA DATA CORRENTE
062500010330     C                   time                    wn14             14 0
062600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062700010330     C                   z-add     wn8           g08dat
062800010330     C                   z-add     *zeros        g08inv
062900010330     C                   movel     '0'           g08err
063000010330     C                   call      'XSRDA8'
063100010330     C                   parm                    wlbda8
063200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063300000613     C*
063400000613     C                   ENDSR
063500000613     C***
063600990908
063700051219     Otitvp7p   E            riepilogo         2
063800990915     O                                              'Upload via Internet'
063900990915     O                                           +1 'Traduzione TIVIN00R -'
064000050408     O                                           +1 'FIVABWWR'
064100990915     O                                           +1 'Testate bolle'
064200990915     O          E            riepilogo   2
064300990915     O                       wrkdata
064400990915     O                       wrkora              +1
064500990915     O                       procname            +1
064600990915     O          E            riepilogo   2
064700990915     O                                              'Cliente..................:'
064800010605     O                       VABCCM        z     +1
064900990915     O          E            riepilogo   2
065000990920     O                                              'Riferimento Strategi.....:'
065100990920     O                       vlrhdl              +1
065200990915     O          E            riepilogo   2
065300990915     O                                              'Giusti...................:'
065400971022     O                       §CTROK        2   +  1
065500990915     O          E            riepilogo   2
065600990915     O                                              'Sbagliati e corretti.....:'
065700971022     O                       §CTRMO        2   +  1
065800990915     O          E            riepilogo   2
065900990915     O                                              'Sbagliati e scartati.....:'
066000971022     O                       §CTRNO        2   +  1
066100000613
066200051219     Otitvp7ps  E            testdett          2
066300000613     O                                              'Upload via Internet'
066400000613     O                                           +1 'Traduzione TIVIN00R -'
066500050408     O                                           +1 'FIVABWWR'
066600000613     O                                           +1 'Testate bolle'
066700000616     O          E            testdett    3
066800000613     O                                           +2 'N° rec'
066900000613     O                                           +3 'Anteprima contenuto'
067000000616     O          E            rigadett    2
067100000613     O                       rrnum               +2
067200000621     O                       recko               +3
067300000616     O          E            findett     2
067400000613     O                       wrkdata
067500000613     O                       wrkora              +1
067600000613     O                       procname            +1
067700000616     O          E            findett     2
067800000613     O                                              'Cliente..................:'
067900010605     O                       VABCCM        z     +1
068000000616     O          E            findett     2
068100000613     O                                              'Riferimento Strategi.....:'
068200000613     O                       vlrhdl              +1
068300000616     O          E            findett     2
068400000613     O                                              'Giusti...................:'
068500000613     O                       §CTROK        2   +  1
068600000616     O          E            findett     2
068700000613     O                                              'Sbagliati e corretti.....:'
068800000613     O                       §CTRMO        2   +  1
068900000616     O          E            findett     2
069000000613     O                                              'Sbagliati e scartati.....:'
069100000613     O                       §CTRNO        2   +  1
069200000613** CMD - COMANDI CL
069300051219OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVP7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
069400051219OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVP7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
069500051219DLTOVR FILE(TITVP7P TITVP7PS) LVL(*)
069600010606
069700000613
