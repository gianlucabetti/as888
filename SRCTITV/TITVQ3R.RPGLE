000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
000700060110     Ftitvq3p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900060110     Ftitvq3ps  O    f  198        PRINTER usropn
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
009300060110     C                   if        not %open(titvq3ps)
009400060110     C                   open      titvq3ps
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
011200060110     C                   if        %open(titvq3ps)
011300000616     C                   except    findett
011400060110     C                   close     titvq3ps
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
013300060110     C                   if        not %open(titvq3p)
013400060110     C                   open      titvq3p
013500990915     C                   endif
013600990915     C*
013700990915     C                   except    riepilogo
013800990915     C*
013900060110     C                   if        %open(titvq3p)
014000060110     C                   close     titvq3p
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
030000060110     C                   EVAL      VABCCM = 0893454
030100060110     C                   EVAL      VABLNP = 089
030200060105     C                   EVAL      VABCTR = 000
030300060110     C                   EVAL      VABTSP = 'C'
030400020204     C* ... e poi verifico se sono stati passati come parametri
030500020204     C                   IF        vlrppt > *blanks
030600040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
030700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030800020204     C                   EXSR      CHKNUM
030900020204     C                   IF        PiInt=*on
031000020204     C                   Z-ADD     PiVal         VABCCM
031100020204     C                   ENDIF
031200040301     C                   ENDIF
031300040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
031400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031500020204     C                   EXSR      CHKNUM
031600020204     C                   IF        PiInt=*on
031700020204     C                   Z-ADD     PiVal         VABLNP
031800020204     C                   ENDIF
031900040301     C                   ENDIF
032000040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
032100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032200020204     C                   EXSR      CHKNUM
032300020204     C                   IF        PiInt=*on
032400020204     C                   Z-ADD     PiVal         VABCTR
032500040301     C                   ENDIF
032600020204     C                   ENDIF
032700020204     C                   ENDIF
032800020204     C*
032900010330     C                   ENDSR
033000010607     C*----------------------------------------------------*
033100050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033200010607     C*----------------------------------------------------*
033300010607     C     IMPVAB        BEGSR
033400010607     C*
033500010607     C                   SETOFF                                       3132
033600010607     C*
033700010607     C* Reperimento campi ALFA
033800010607     C*
033900010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034000060126     C                   EVAL      VABRSD=%trim(%subst(vindta:183:37))
034100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034300020117     C* ==
034400060126     C                   EVAL      VABIND=%trim(%subst(vindta:220:35))
034500060126     C                   EVAL      VABLOD=%trim(%subst(vindta:255:35))
034600060126     C                   EVAL      VABPRD=%trim(%subst(vindta:295:2))
034700060126     C                   EVAL      VABRMA=%trim(%subst(vindta:304:5))
034800060126     C                   IF        %trim(%subst(vindta:404:2)) = '01'
034900060126     C                   EVAL      VABCBO = '1'
035000060126     C                   ENDIF
035100060126     C                   IF        %trim(%subst(vindta:404:2)) = '02'
035200060126     C                   EVAL      VABCBO = '2'
035300060126     C                   ENDIF
035400060126     C                   IF        %trim(%subst(vindta:404:2)) = '03'
035500060208     C                   EVAL      VABCBO = '1'
035600060126     C                   ENDIF
035700010607     C*
035800010607     C* Reperimento campi NUMERICI
035900020722     C                   MOVEL     DATCOR        VABAAS
036000020722     C                   MOVE      DATCOR        VABMGS
036100060105     C* RMN/NSP
036200060126     C                   EVAL      PiStr=%trim(%subst(vindta:304:5))
036300050408     C                   EXSR      CHKNUM
036400050408     C                   IF        PiInt=*on
036500050408     C                   Z-ADD     PiVal         VABRMN
036600050908     C                   Z-ADD     PiVal         VABNSP
036700050408     C                   ELSE
036800051219     C                   SETON                                        31
036900050408     C                   Z-ADD     *zeros        VABRMN
037000050408     C                   EVAL      vinmsg = %trimr(vinmsg)
037100050908     C                             + ' ' + 'VABRMN VABNSP'
037200050408     C                   ENDIF
037300040420     C* CAD
037400060126     C                   EVAL      PiStr=%trim(%subst(vindta:290:5))
037500010607     C                   EXSR      CHKNUM
037600010607     C                   IF        PiInt=*on
037700020204     C                   Z-ADD     PiVal         Num5_0
037800020204     C                   MOVEL(p)  Num5_0        VABCAD
037900010607     C                   ELSE
038000010607     C                   SETON                                        32
038100020204     C                   EVAL      VABCAD = *zeros
038200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038300020204     C                             + ' ' + 'VABCAD'
038400010607     C                   ENDIF
038500040420     C* NCL
038600060126     C                   EVAL      PiStr=%trim(%subst(vindta:332:2))
038700010607     C                   EXSR      CHKNUM
038800010607     C                   IF        PiInt=*on
038900010607     C                   Z-ADD     PiVal         VABNCL
039000010607     C                   ELSE
039100010607     C                   SETON                                        32
039200010607     C                   Z-ADD     *zeros        VABNCL
039300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039400010607     C                             + ' ' + 'VABNCL'
039500010607     C                   ENDIF
039600040420     C* PKB
039700060126     C                   EVAL      PiStr=%trim(%subst(vindta:320:7))
039800010607     C                   EXSR      CHKNUM
039900010607     C                   IF        PiNum=*on
040000050330     C                   Z-ADD(H)  PiVal         VABPKB
040100010607     C                   ELSE
040200010607     C                   SETON                                        32
040300010607     C                   Z-ADD     *zeros        VABPKB
040400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
040500010607     C                             + ' ' + 'VABPKB'
040600010607     C                   ENDIF
040700020322     C*
040800020322     C* Considerazioni finali su CBO/CAS
040900050908     C                   IF        FlgCAS = '1'
041000040713     C                   IF        VABCBO = '1'
041100020322     C                   EVAL      VABCBO = '4'
041200040713     C                   ENDIF
041300040713     C                   IF        VABCBO = '2'
041400040713     C                   EVAL      VABCBO = '6'
041500040713     C                   ENDIF
041600020322     C                   ENDIF
041700020204     C*
041800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041900020204     C                   EXSR      CHKIMPDIV
042000020204     C*
042100010607     C                   ENDSR
042200010607     C*----------------------------------------------------*
042300010601
042400020204
042500020204     C*----------------------------------------------------*
042600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
042700020204     C*----------------------------------------------------*
042800020204     C     CHKIMPDIV     BEGSR
042900020204     C*
043000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
043100020204     C                   Z-ADD     *zeros        wrkDec            9 9
043200020204     C*
043300020204     C* Come prima cosa effettuo considerazioni sulla divisa
043400020204     C                   IF        vabIAS > *zeros
043500020204     C                   IF        vabVAS <> 'EUR'
043600020204     C                   EVAL      vabVAS =  'ITL'
043700020204     C                   ENDIF
043800020204     C                   ENDIF
043900020204     C*
044000020204     C                   IF        vabCAS > *zeros
044100020204     C                   IF        vabVCA <> 'EUR'
044200020204     C                   EVAL      vabVCA =  'ITL'
044300020204     C                   ENDIF
044400020204     C                   ENDIF
044500020204     C*
044600020204     C                   IF        vabVMD > *zeros
044700020321     C                   IF        vabVAD <> 'EUR'
044800020204     C                   EVAL      vabVAD =  'ITL'
044900020204     C                   ENDIF
045000020204     C                   ENDIF
045100020204     C*
045200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
045300020204     C                   Z-ADD     vabIAS        wrkDec
045400020204     C                   IF        wrkDec > *zeros
045500020204     C                   IF        vabVAS = 'ITL'
045600020204     C                   EVAL      vabIAS = *zeros
045700020204     C                   ENDIF
045800020204     C                   ENDIF
045900020204     C*
046000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
046100020204     C                   Z-ADD     vabCAS        wrkDec
046200020204     C                   IF        wrkDec > *zeros
046300020204     C                   IF        vabVCA = 'ITL'
046400020204     C                   EVAL      vabCAS = *zeros
046500020204     C                   ENDIF
046600020204     C                   ENDIF
046700020204     C*
046800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
046900020204     C                   Z-ADD     vabVMD        wrkDec
047000020204     C                   IF        wrkDec > *zeros
047100020204     C                   IF        vabVAD = 'ITL'
047200020204     C                   EVAL      vabVMD = *zeros
047300020204     C                   ENDIF
047400020204     C                   ENDIF
047500020204     C*
047600020204     C                   ENDSR
047700020204     C***
047800020204
047900010330
048000010330
048100010330     C*----------------------------------------------------*
048200010330     C*  CONTROLLO NUMERICITA' CAMPI
048300010330     C*----------------------------------------------------*
048400010330     C     CHKNUM        BEGSR
048500010330     C*
048600010606     C                   IF        PiDecChr = *blanks
048700010606     C                   EVAL      PiDecChr = ','
048800010606     C                   ENDIF
048900010606     C*
049000010606     C                   CALL(e)   'ISNUMERIC'
049100010330     C                   PARM                    PiStr            30
049200010606     C                   PARM                    PiDecChr          1
049300010330     C                   PARM      *ZEROS        PiVal            30 9
049400010330     C                   PARM      '0'           PiInt             1
049500010330     C                   PARM      '0'           PiNum             1
049600010330     C                   IF        %error
049700010606     C                   EVAL      PiNum=*off
049800010330     C                   ENDIF
049900010330     C*
050000010330     C                   ENDSR
050100010330     C***
050200010330
050300010601
050400010601
050500010601
050600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
050700010601     C     repfil        BEGSR
050800010601     C*
050900010601     C                   if        invfil = *zeros and
051000010601     C                             depfil > *zeros and
051100010629     C                             (vinflg = *blanks or
051200010629     C                              vinflg = *zeros)
051300010601     C
051400010601     C                   eval      invfil = depfil
051500010601     C                   endif
051600010601     C*
051700010601     C                   if        depfil <> invfil and
051800010601     C                             invfil > *zeros
051900010601     C                   eval      flgMulti = '1'
052000010601     C                   if        vinflg = *blanks
052100010601     C                   add       1             cntNonEl
052200010601     C                   endif
052300010601     C                   endif
052400010601     C*
052500010601     C                   if        vinflg = '2'
052600010601     C                   eval      flgStato = '2'
052700010601     C                   endif
052800010601     C*
052900010601     C                   ENDSR
053000010601     C***
053100010601
053200010601
053300010601
053400010330
053500010330
053600010330
053700990920      /TITLE Invio dei dati al punto operativo.
053800000613     C     invio         BEGSR
053900990920     C*
054000990920     C                   reset                   dscmz
054100010601     C                   move      invfil        cmzdst
054200990920     C                   eval      cmzfld = vlrfou
054300990920     C                   eval      cmzmbd = vlrhdl
054400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
054500000710     C                   if        prmfir = *blanks
054600050408     C                   eval      cmzfla = 'FIVAB00F'
054700050408     C                   eval      cmzmba = 'FIVAB00F'
054800000710     C                   else
054900000710     C                   eval      cmzfla = prmfir
055000000710     C                   eval      cmzmba = prmfir
055100000710     C                   endif
055200990920     C                   eval      cmznrr = *zeros
055300990920     C                   move      §ctrok        cmznrr
055400021018     C                   eval      cmzlba = vlrfl1
055500990920     C                   call(e)   'TIS711C'
055600990920     C                   parm                    dscmz
055700990921     C                   parm      *blanks       esito
055800990920     C                   if        %error
055900990920     C                             or cmzerr = '1'
056000990921     C                             or esito  = '1'
056100000710     C                   eval      wrkesito = '3'
056200990920     C                   endif
056300990920     C*
056400000613     C                   ENDSR
056500990910
056600010601
056700010601
056800010601
056900010601
057000010601      /TITLE Invio dei dati al punto operativo.
057100010601     C     opeini        BEGSR
057200010601     C*
057300010601     C* Inizializzo flag e contatori operativi
057400010601     C                   movel     '0'           flgGiro           1
057500010601     C                   movel     '0'           flgMulti          1
057600010601     C                   movel     '1'           flgStato          1
057700010615     C                   movel     '0'           flgOk             1
057800010601     C                   z-add     *zeros        cntNonEl         10 0
057900010601     C                   z-add     *zeros        depfil            3 0
058000010601     C                   z-add     *zeros        invfil            3 0
058100010601     C*
058200010601     C                   ENDSR
058300010601     C***
058400010601
058500010601
058600010601
058700010330
058800010330
058900000613     C     *inzsr        BEGSR
059000990910     C*
059100990910     C     *entry        plist
059200990920     C                   parm                    tivlrds
059300990921     C                   parm      wrkesito      esito
059400000724     C                   parm                    prmlit
059500000710     C                   parm                    prmfir
059600010330     C*
059700010330     C* CALCOLA LA DATA CORRENTE
059800010330     C                   time                    wn14             14 0
059900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
060000010330     C                   z-add     wn8           g08dat
060100010330     C                   z-add     *zeros        g08inv
060200010330     C                   movel     '0'           g08err
060300010330     C                   call      'XSRDA8'
060400010330     C                   parm                    wlbda8
060500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
060600000613     C*
060700000613     C                   ENDSR
060800000613     C***
060900990908
061000060110     Otitvq3p   E            riepilogo         2
061100990915     O                                              'Upload via Internet'
061200990915     O                                           +1 'Traduzione TIVIN00R -'
061300050408     O                                           +1 'FIVABWWR'
061400990915     O                                           +1 'Testate bolle'
061500990915     O          E            riepilogo   2
061600990915     O                       wrkdata
061700990915     O                       wrkora              +1
061800990915     O                       procname            +1
061900990915     O          E            riepilogo   2
062000990915     O                                              'Cliente..................:'
062100010605     O                       VABCCM        z     +1
062200990915     O          E            riepilogo   2
062300990920     O                                              'Riferimento Strategi.....:'
062400990920     O                       vlrhdl              +1
062500990915     O          E            riepilogo   2
062600990915     O                                              'Giusti...................:'
062700971022     O                       §CTROK        2   +  1
062800990915     O          E            riepilogo   2
062900990915     O                                              'Sbagliati e corretti.....:'
063000971022     O                       §CTRMO        2   +  1
063100990915     O          E            riepilogo   2
063200990915     O                                              'Sbagliati e scartati.....:'
063300971022     O                       §CTRNO        2   +  1
063400000613
063500060110     Otitvq3ps  E            testdett          2
063600000613     O                                              'Upload via Internet'
063700000613     O                                           +1 'Traduzione TIVIN00R -'
063800050408     O                                           +1 'FIVABWWR'
063900000613     O                                           +1 'Testate bolle'
064000000616     O          E            testdett    3
064100000613     O                                           +2 'N° rec'
064200000613     O                                           +3 'Anteprima contenuto'
064300000616     O          E            rigadett    2
064400000613     O                       rrnum               +2
064500000621     O                       recko               +3
064600000616     O          E            findett     2
064700000613     O                       wrkdata
064800000613     O                       wrkora              +1
064900000613     O                       procname            +1
065000000616     O          E            findett     2
065100000613     O                                              'Cliente..................:'
065200010605     O                       VABCCM        z     +1
065300000616     O          E            findett     2
065400000613     O                                              'Riferimento Strategi.....:'
065500000613     O                       vlrhdl              +1
065600000616     O          E            findett     2
065700000613     O                                              'Giusti...................:'
065800000613     O                       §CTROK        2   +  1
065900000616     O          E            findett     2
066000000613     O                                              'Sbagliati e corretti.....:'
066100000613     O                       §CTRMO        2   +  1
066200000616     O          E            findett     2
066300000613     O                                              'Sbagliati e scartati.....:'
066400000613     O                       §CTRNO        2   +  1
066500000613** CMD - COMANDI CL
066600060110OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVQ3P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
066700060110OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVQ3PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
066800060110DLTOVR FILE(TITVQ3P TITVQ3PS) LVL(*)
066900010606
067000000613
