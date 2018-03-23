000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700060303     Ftitvr5p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900060303     Ftitvr5ps  O    f  198        PRINTER usropn
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
009300060303     C                   if        not %open(titvr5ps)
009400060303     C                   open      titvr5ps
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
011200060303     C                   if        %open(titvr5ps)
011300000616     C                   except    findett
011400060303     C                   close     titvr5ps
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
013300060303     C                   if        not %open(titvr5p)
013400060303     C                   open      titvr5p
013500990915     C                   endif
013600990915     C*
013700990915     C                   except    riepilogo
013800990915     C*
013900060303     C                   if        %open(titvr5p)
014000060303     C                   close     titvr5p
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
030000060303     C                   MOVEL     *blanks       wKSC              7
030100060303     C                   EVAL      wKSC   = %subst(vlrksc:2:7)
030200060303     C                   MOVE(P)   wKSC          VABCCM
030300060303     C                   IF        VABCCM = 0450200
030400060303     C                   EVAL      VABLNP = 202
030500060303     C                   ELSE
030600060303     C                   EVAL      VABLNP = 045
030700060303     C                   ENDIF
030800060105     C                   EVAL      VABCTR = 000
030900060303     C                   EVAL      VABCBO = '1'
031000020204     C* ... e poi verifico se sono stati passati come parametri
031100020204     C                   IF        vlrppt > *blanks
031200040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031400020204     C                   EXSR      CHKNUM
031500020204     C                   IF        PiInt=*on
031600020204     C                   Z-ADD     PiVal         VABCCM
031700020204     C                   ENDIF
031800040301     C                   ENDIF
031900040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
032000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032100020204     C                   EXSR      CHKNUM
032200020204     C                   IF        PiInt=*on
032300020204     C                   Z-ADD     PiVal         VABLNP
032400020204     C                   ENDIF
032500040301     C                   ENDIF
032600040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
032700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032800020204     C                   EXSR      CHKNUM
032900020204     C                   IF        PiInt=*on
033000020204     C                   Z-ADD     PiVal         VABCTR
033100040301     C                   ENDIF
033200020204     C                   ENDIF
033300020204     C                   ENDIF
033400020204     C*
033500010330     C                   ENDSR
033600010607     C*----------------------------------------------------*
033700050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033800010607     C*----------------------------------------------------*
033900010607     C     IMPVAB        BEGSR
034000010607     C*
034100010607     C                   SETOFF                                       3132
034200010607     C*
034300010607     C* Reperimento campi ALFA
034400010607     C*
034500010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034600060303     C                   EVAL      VABRSD=%trim(%subst(vindta:17:35))
034700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034900020117     C* ==
035000060303     C                   EVAL      VABIND=%trim(%subst(vindta:52:35))
035100060303     C                   EVAL      VABLOD=%trim(%subst(vindta:87:22))
035200060303     C                   EVAL      VABPRD=%trim(%subst(vindta:109:2))
035300060303     C                   EVAL      VABRMA=%trim(%subst(vindta:1:6))
035400060303     C                   MOVEL     *blanks       wNOTE           200
035500060303     C                   EVAL      wNOTE =%trim(%subst(vindta:145:200))
035600060303     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
035700060303     C                   EVAL      VABNT2=%trim(%subst(wNOTE:1+35:35))
035800010607     C*
035900010607     C* Reperimento campi NUMERICI
036000020722     C                   MOVEL     DATCOR        VABAAS
036100020722     C                   MOVE      DATCOR        VABMGS
036200060105     C* RMN/NSP
036300060303     C                   EVAL      PiStr=%trim(%subst(vindta:139:6))
036400050408     C                   EXSR      CHKNUM
036500050408     C                   IF        PiInt=*on
036600050408     C                   Z-ADD     PiVal         VABRMN
036700050908     C                   Z-ADD     PiVal         VABNSP
036800050408     C                   ELSE
036900051219     C                   SETON                                        31
037000050408     C                   Z-ADD     *zeros        VABRMN
037100050408     C                   EVAL      vinmsg = %trimr(vinmsg)
037200050908     C                             + ' ' + 'VABRMN VABNSP'
037300050408     C                   ENDIF
037400040420     C* CAD
037500060303     C                   EVAL      PiStr=%trim(%subst(vindta:111:9))
037600010607     C                   EXSR      CHKNUM
037700010607     C                   IF        PiInt=*on
037800020204     C                   Z-ADD     PiVal         Num5_0
037900020204     C                   MOVEL(p)  Num5_0        VABCAD
038000010607     C                   ELSE
038100010607     C                   SETON                                        32
038200020204     C                   EVAL      VABCAD = *zeros
038300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038400020204     C                             + ' ' + 'VABCAD'
038500010607     C                   ENDIF
038600040420     C* NCL
038700060303     C                   EVAL      PiStr=%trim(%subst(vindta:121:3))
038800010607     C                   EXSR      CHKNUM
038900010607     C                   IF        PiInt=*on
039000010607     C                   Z-ADD     PiVal         VABNCL
039100010607     C                   ELSE
039200010607     C                   SETON                                        32
039300010607     C                   Z-ADD     *zeros        VABNCL
039400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039500010607     C                             + ' ' + 'VABNCL'
039600010607     C                   ENDIF
039700040420     C* PKB
039800060303     C                   EVAL      PiStr=%trim(%subst(vindta:126:11))
039900010607     C                   EXSR      CHKNUM
040000010607     C                   IF        PiNum=*on
040100060303     C                   EVAL(H)   VABPKB = PiVal / 1000                        * da grammi a kg
040200010607     C                   ELSE
040300010607     C                   SETON                                        32
040400010607     C                   Z-ADD     *zeros        VABPKB
040500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040600010607     C                             + ' ' + 'VABPKB'
040700010607     C                   ENDIF
040800020322     C*
040900020322     C* Considerazioni finali su CBO/CAS
041000050908     C                   IF        FlgCAS = '1'
041100040713     C                   IF        VABCBO = '1'
041200020322     C                   EVAL      VABCBO = '4'
041300040713     C                   ENDIF
041400040713     C                   IF        VABCBO = '2'
041500040713     C                   EVAL      VABCBO = '6'
041600040713     C                   ENDIF
041700020322     C                   ENDIF
041800020204     C*
041900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042000020204     C                   EXSR      CHKIMPDIV
042100020204     C*
042200010607     C                   ENDSR
042300010607     C*----------------------------------------------------*
042400010601
042500020204
042600020204     C*----------------------------------------------------*
042700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
042800020204     C*----------------------------------------------------*
042900020204     C     CHKIMPDIV     BEGSR
043000020204     C*
043100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
043200020204     C                   Z-ADD     *zeros        wrkDec            9 9
043300020204     C*
043400020204     C* Come prima cosa effettuo considerazioni sulla divisa
043500020204     C                   IF        vabIAS > *zeros
043600020204     C                   IF        vabVAS <> 'EUR'
043700020204     C                   EVAL      vabVAS =  'ITL'
043800020204     C                   ENDIF
043900020204     C                   ENDIF
044000020204     C*
044100020204     C                   IF        vabCAS > *zeros
044200020204     C                   IF        vabVCA <> 'EUR'
044300020204     C                   EVAL      vabVCA =  'ITL'
044400020204     C                   ENDIF
044500020204     C                   ENDIF
044600020204     C*
044700020204     C                   IF        vabVMD > *zeros
044800020321     C                   IF        vabVAD <> 'EUR'
044900020204     C                   EVAL      vabVAD =  'ITL'
045000020204     C                   ENDIF
045100020204     C                   ENDIF
045200020204     C*
045300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
045400020204     C                   Z-ADD     vabIAS        wrkDec
045500020204     C                   IF        wrkDec > *zeros
045600020204     C                   IF        vabVAS = 'ITL'
045700020204     C                   EVAL      vabIAS = *zeros
045800020204     C                   ENDIF
045900020204     C                   ENDIF
046000020204     C*
046100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
046200020204     C                   Z-ADD     vabCAS        wrkDec
046300020204     C                   IF        wrkDec > *zeros
046400020204     C                   IF        vabVCA = 'ITL'
046500020204     C                   EVAL      vabCAS = *zeros
046600020204     C                   ENDIF
046700020204     C                   ENDIF
046800020204     C*
046900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
047000020204     C                   Z-ADD     vabVMD        wrkDec
047100020204     C                   IF        wrkDec > *zeros
047200020204     C                   IF        vabVAD = 'ITL'
047300020204     C                   EVAL      vabVMD = *zeros
047400020204     C                   ENDIF
047500020204     C                   ENDIF
047600020204     C*
047700020204     C                   ENDSR
047800020204     C***
047900020204
048000010330
048100010330
048200010330     C*----------------------------------------------------*
048300010330     C*  CONTROLLO NUMERICITA' CAMPI
048400010330     C*----------------------------------------------------*
048500010330     C     CHKNUM        BEGSR
048600010330     C*
048700010606     C                   IF        PiDecChr = *blanks
048800010606     C                   EVAL      PiDecChr = ','
048900010606     C                   ENDIF
049000010606     C*
049100010606     C                   CALL(e)   'ISNUMERIC'
049200010330     C                   PARM                    PiStr            30
049300010606     C                   PARM                    PiDecChr          1
049400010330     C                   PARM      *ZEROS        PiVal            30 9
049500010330     C                   PARM      '0'           PiInt             1
049600010330     C                   PARM      '0'           PiNum             1
049700010330     C                   IF        %error
049800010606     C                   EVAL      PiNum=*off
049900010330     C                   ENDIF
050000010330     C*
050100010330     C                   ENDSR
050200010330     C***
050300010330
050400010601
050500010601
050600010601
050700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
050800010601     C     repfil        BEGSR
050900010601     C*
051000010601     C                   if        invfil = *zeros and
051100010601     C                             depfil > *zeros and
051200010629     C                             (vinflg = *blanks or
051300010629     C                              vinflg = *zeros)
051400010601     C
051500010601     C                   eval      invfil = depfil
051600010601     C                   endif
051700010601     C*
051800010601     C                   if        depfil <> invfil and
051900010601     C                             invfil > *zeros
052000010601     C                   eval      flgMulti = '1'
052100010601     C                   if        vinflg = *blanks
052200010601     C                   add       1             cntNonEl
052300010601     C                   endif
052400010601     C                   endif
052500010601     C*
052600010601     C                   if        vinflg = '2'
052700010601     C                   eval      flgStato = '2'
052800010601     C                   endif
052900010601     C*
053000010601     C                   ENDSR
053100010601     C***
053200010601
053300010601
053400010601
053500010330
053600010330
053700010330
053800990920      /TITLE Invio dei dati al punto operativo.
053900000613     C     invio         BEGSR
054000990920     C*
054100990920     C                   reset                   dscmz
054200010601     C                   move      invfil        cmzdst
054300990920     C                   eval      cmzfld = vlrfou
054400990920     C                   eval      cmzmbd = vlrhdl
054500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
054600000710     C                   if        prmfir = *blanks
054700050408     C                   eval      cmzfla = 'FIVAB00F'
054800050408     C                   eval      cmzmba = 'FIVAB00F'
054900000710     C                   else
055000000710     C                   eval      cmzfla = prmfir
055100000710     C                   eval      cmzmba = prmfir
055200000710     C                   endif
055300990920     C                   eval      cmznrr = *zeros
055400990920     C                   move      §ctrok        cmznrr
055500021018     C                   eval      cmzlba = vlrfl1
055600990920     C                   call(e)   'TIS711C'
055700990920     C                   parm                    dscmz
055800990921     C                   parm      *blanks       esito
055900990920     C                   if        %error
056000990920     C                             or cmzerr = '1'
056100990921     C                             or esito  = '1'
056200000710     C                   eval      wrkesito = '3'
056300990920     C                   endif
056400990920     C*
056500000613     C                   ENDSR
056600990910
056700010601
056800010601
056900010601
057000010601
057100010601      /TITLE Invio dei dati al punto operativo.
057200010601     C     opeini        BEGSR
057300010601     C*
057400010601     C* Inizializzo flag e contatori operativi
057500010601     C                   movel     '0'           flgGiro           1
057600010601     C                   movel     '0'           flgMulti          1
057700010601     C                   movel     '1'           flgStato          1
057800010615     C                   movel     '0'           flgOk             1
057900010601     C                   z-add     *zeros        cntNonEl         10 0
058000010601     C                   z-add     *zeros        depfil            3 0
058100010601     C                   z-add     *zeros        invfil            3 0
058200010601     C*
058300010601     C                   ENDSR
058400010601     C***
058500010601
058600010601
058700010601
058800010330
058900010330
059000000613     C     *inzsr        BEGSR
059100990910     C*
059200990910     C     *entry        plist
059300990920     C                   parm                    tivlrds
059400990921     C                   parm      wrkesito      esito
059500000724     C                   parm                    prmlit
059600000710     C                   parm                    prmfir
059700010330     C*
059800010330     C* CALCOLA LA DATA CORRENTE
059900010330     C                   time                    wn14             14 0
060000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
060100010330     C                   z-add     wn8           g08dat
060200010330     C                   z-add     *zeros        g08inv
060300010330     C                   movel     '0'           g08err
060400010330     C                   call      'XSRDA8'
060500010330     C                   parm                    wlbda8
060600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
060700000613     C*
060800000613     C                   ENDSR
060900000613     C***
061000990908
061100060303     Otitvr5p   E            riepilogo         2
061200990915     O                                              'Upload via Internet'
061300990915     O                                           +1 'Traduzione TIVIN00R -'
061400050408     O                                           +1 'FIVABWWR'
061500990915     O                                           +1 'Testate bolle'
061600990915     O          E            riepilogo   2
061700990915     O                       wrkdata
061800990915     O                       wrkora              +1
061900990915     O                       procname            +1
062000990915     O          E            riepilogo   2
062100990915     O                                              'Cliente..................:'
062200010605     O                       VABCCM        z     +1
062300990915     O          E            riepilogo   2
062400990920     O                                              'Riferimento Strategi.....:'
062500990920     O                       vlrhdl              +1
062600990915     O          E            riepilogo   2
062700990915     O                                              'Giusti...................:'
062800971022     O                       §CTROK        2   +  1
062900990915     O          E            riepilogo   2
063000990915     O                                              'Sbagliati e corretti.....:'
063100971022     O                       §CTRMO        2   +  1
063200990915     O          E            riepilogo   2
063300990915     O                                              'Sbagliati e scartati.....:'
063400971022     O                       §CTRNO        2   +  1
063500000613
063600060303     Otitvr5ps  E            testdett          2
063700000613     O                                              'Upload via Internet'
063800000613     O                                           +1 'Traduzione TIVIN00R -'
063900050408     O                                           +1 'FIVABWWR'
064000000613     O                                           +1 'Testate bolle'
064100000616     O          E            testdett    3
064200000613     O                                           +2 'N° rec'
064300000613     O                                           +3 'Anteprima contenuto'
064400000616     O          E            rigadett    2
064500000613     O                       rrnum               +2
064600000621     O                       recko               +3
064700000616     O          E            findett     2
064800000613     O                       wrkdata
064900000613     O                       wrkora              +1
065000000613     O                       procname            +1
065100000616     O          E            findett     2
065200000613     O                                              'Cliente..................:'
065300010605     O                       VABCCM        z     +1
065400000616     O          E            findett     2
065500000613     O                                              'Riferimento Strategi.....:'
065600000613     O                       vlrhdl              +1
065700000616     O          E            findett     2
065800000613     O                                              'Giusti...................:'
065900000613     O                       §CTROK        2   +  1
066000000616     O          E            findett     2
066100000613     O                                              'Sbagliati e corretti.....:'
066200000613     O                       §CTRMO        2   +  1
066300000616     O          E            findett     2
066400000613     O                                              'Sbagliati e scartati.....:'
066500000613     O                       §CTRNO        2   +  1
066600000613** CMD - COMANDI CL
066700060303OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVR5P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
066800060303OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVR5PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
066900060303DLTOVR FILE(TITVR5P TITVR5PS) LVL(*)
067000010606
067100000613
