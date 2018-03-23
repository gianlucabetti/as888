000100021025      /TITLE Upload via Internet: traduzione in FIVABWWR. (x ZEIS e CALENDAR)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700020829     Ftitv22p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900020829     Ftitv22ps  O    f  198        PRINTER usropn
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
004000020829     D*------------------
004100020829     D* DS DI RIDEFINIZIONE DATI DI INPUT
004200020829     D*------------------
004300020829     D DSINPUT         DS                  INZ
004400020829     D  TIPREC                 3      6
004500020829     D  RAG                   19     55
004600020829     D  IND                   56     81
004700020829     D  CAP                   82     86
004800020829     D  LOC                   89    119
004900020829     D  PRV                  120    121
005000020829     D  COL                  129    132
005100020829     D  RIFNUM                 8     12
005200020829     D*
005300010330
005400010330
005500010330
005600990915     C                   time                    wrkdata
005700990915     C                   time                    wrkora
005800000913     C                   reset                   rrnum
005900990921     C                   reset                   esito
006000990921     C                   reset                   wrkesito
006100010601     C*
006200010601     C                   exsr      opeini
006300010605     C                   exsr      rwvab
006400010601     C*
006500010601     C                   seton                                        lr
006600010601
006700010601
006800010601
006900010601
007000010601     C*--------------------------------------------------------
007100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007200010601     C*--------------------------------------------------------
007300010601     C     PREELA        BEGSR
007400010601     C*
007500010601     C* SE OCCORRE SPEDIRE IN FILIALE
007600010601     C                   if        invfil <> *zeros and
007700010601     C                             flgGiro = '0'
007800010601     C*
007900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008000010601     C                   eval      flgGiro = '1'
008100000724     C*
008200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008400010601     C     invfil        chain     azorg01l
008500000724     C                   if        %found
008600000724     C                   movel(p)  CMD(1)        depcmd
008700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008800000724     C*
008900000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009000011113     C                   Z-ADD     150           LENGH            15 5
009100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009200000724     C                   PARM                    depcmd
009300000724     C                   PARM                    LENGH
009400000724     C*
009500000724     C                   endif
009600000616     C*
009700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009800000616     C                   movel(p)  CMD(2)        depcmd
009900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010000000616     C*
010100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010200011113     C                   Z-ADD     150           LENGH            15 5
010300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010400000616     C                   PARM                    depcmd
010500000616     C                   PARM                    LENGH
010600000613     C*
010700020829     C                   if        not %open(titv22ps)
010800020829     C                   open      titv22ps
010900000616     C                   except    testdett
011000000613     C                   endif
011100010601     C*
011200010601     C                   endif
011300010601     C*
011400010601     C                   ENDSR
011500010601     C***
011600010601
011700010601
011800010601
011900010601
012000010601
012100010601
012200010601
012300010601     C*--------------------------------------------------------
012400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012500010601     C*--------------------------------------------------------
012600010601     C     ENDELA        BEGSR
012700990915     C*
012800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012900000613     C*
013000020829     C                   if        %open(titv22ps)
013100000616     C                   except    findett
013200020829     C                   close     titv22ps
013300000613     C                   endif
013400000616     C*
013500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013600011113     C                   Z-ADD     150           LENGH            15 5
013700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013800000616     C                   PARM                    CMD(3)
013900000616     C                   PARM                    LENGH
014000000616     C*
014100010601     C                   ENDSR
014200010601     C***
014300010601
014400010601
014500010601
014600010601
014700000613
014800000613     C*--------------------------------------------------------
014900000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015000000613     C*--------------------------------------------------------
015100000621     C     STPR          BEGSR
015200000613     C*
015300020829     C                   if        not %open(titv22p)
015400020829     C                   open      titv22p
015500990915     C                   endif
015600990915     C*
015700990915     C                   except    riepilogo
015800990915     C*
015900020829     C                   if        %open(titv22p)
016000020829     C                   close     titv22p
016100990914     C                   endif
016200000613     C*
016300000613     C                   ENDSR
016400000613     C***
016500000613
016600010330
016700010601
016800010601
016900010601
017000010330     C*--------------------------------------------------------
017100021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
017200010330     C*--------------------------------------------------------
017300010605     C     RWVAB         BEGSR
017400010330     C*
017500010330     C                   if        not %open(tivin00r)
017600010330     C                   open      tivin00r
017700010330     C                   endif
017800021025     C                   if        not %open(fivabwwr)
017900021025     C                   open      fivabwwr
018000010330     C                   endif
018100010330     C*
018200010604     C                   clear                   §CTROK
018300010604     C                   clear                   §CTRMO
018400010604     C                   clear                   §CTRNO
018500010330     C*
018600010330     C                   DO        *HIVAL
018700010330     C*
018800010330     C                   READ      tivin00r                               70
018900010618     C*
019000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019100010618     C                   if        vinflg = '1'
019200010618     C                   eval      flgOk = '1'
019300010618     C                   endif
019400010618     C*
019500010330     C                   if        vindta > *blanks
019600010330     C                   add       1             rrnum
019700020829     C                   movel(p)  vindta        dsinput
019800020829     C*
019900020829     C     TIPREC        IFEQ      'VEND'
020000020829     C     TIPREC        OREQ      'TRAS'
020100020829     C     TIPREC        OREQ      'SOST'
020200020829     C     TIPREC        OREQ      'RIPA'
020300020829     C     TIPREC        OREQ      'RESO'
020400020829     C     TIPREC        OREQ      'CVIS'
020500020829     C     TIPREC        OREQ      'PUBB'
020600010330     C*
020700010601     C                   if        *in70 = *off and
020800010330     C                             (vinflg = *blanks
020900010330     C                              or vinflg = '0'
021000010330     C                              or vinflg = '2')
021100010330     C*
021200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021300010711     C                   if        vinflg = *blanks or vinflg = '0'
021400010711     C                   clear                   vinmsg
021500010711     C                   endif
021600010601     C*
021700010330     C                   exsr      inzvar
021800010330     C                   exsr      defcam
021900010605     C                   exsr      impvab
022000010601     C*
022100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022200010605     C                   eval      depfil = VABLNP
022300010601     C                   exsr      repfil
022400010601     C                   if        depfil = invfil
022500021025     C                   if        vlrpoi = 999
022600021025     C                   MOVE(P)   invfil        VABFGS
022700021025     C                   else
022800021025     C                   MOVE(P)   vlrpoi        VABFGS
022900021025     C                   endif
023000010601     C*
023100010601     C                   exsr      PREELA
023200010601     C*
023300010604     C* Ebbene...
023400010604     C*
023500010604     C  N31              ADD       1             §CTROK            7 0
023600010604     C   32              ADD       1             §CTRMO            7 0
023700010604     C   31              ADD       1             §CTRNO            7 0
023800020205     C*
023900020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
024000021025     C  N31              WRITE     FIVAB000
024100020722     C*
024200010604     C                   if        *in31 = *off and
024300010604     C                             *in32 = *off
024400010604     C                   eval      vinflg = '1'
024500010604     C                   else
024600010604     C                   eval      recko = vindta
024700010604     C                   except    rigadett
024800010604     C                   eval      vinflg = '2'
024900010604     C                   endif
025000010604     C                   endif
025100010601     C*
025200010604     C                   endif
025300020829     C*
025400020829     C                   else
025500020829     C                   eval      vinflg = '1'
025600020829     C                   endif
025700010604     C*
025800010330     C                   else
025900010330     C                   eval      vinflg = '1'
026000010330     C                   endif
026100010601     C*
026200010601     C  N70              update    tivin000
026300010330     C*
026400010330     C  N70              ENDdo
026500010601     C
026600010601     C*
026700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026800010601     C                   if        cntNonEl = *zeros or
026900010601     C                             flgMulti = '0'
027000010330     C* Se non ci sono record con errori ...
027100010601     C                   if        §ctrno = 0 and
027200010604     C                             §ctrmo = 0 and
027300010601     C                             flgStato <> '2'
027400010330     C* ... restituisco esito OK.
027500010330     C                   eval      wrkesito = '0'
027600010330     C                   else
027700010330     C                   if        §ctrok > 0
027800010330     C                   eval      wrkesito = '1'
027900010330     C                   else
028000010615     C                   if        flgOk = '0'
028100010615     C                   eval      wrkesito = '2'
028200010615     C                   else
028300010615     C                   eval      wrkesito = '6'
028400010615     C                   endif
028500010330     C                   endif
028600010330     C                   endif
028700010601     C                   else
028800010601     C                   eval      wrkesito = '9'
028900010601     C                   endif
029000010330     C*
029100010330     C                   if        %open(tivin00r)
029200010330     C                   close     tivin00r
029300010330     C                   endif
029400021025     C                   if        %open(fivabwwr)
029500021025     C                   close     fivabwwr
029600010330     C                   endif
029700010601     C*
029800010601     C                   if        vlrpoi <> 999
029900010601     C                   eval      invfil = vlrpoi
030000010601     C                   endif
030100010330     C*
030200010330     C                   if        §ctrok > 0
030300010601     C                             and invfil > *zeros
030400010330     C                   exsr      invio
030500010330     C                   endif
030600010601     C*
030700010618     C                   if        flgGiro = '1'
030800010601     C                   exsr      endela
030900010618     C                   endif
031000010330     C*
031100010330     C                   ENDSR
031200010330     C***
031300010330
031400010601
031500010601
031600010601
031700010601
031800010330     C*----------------------------------------------------*
031900020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032000010330     C*----------------------------------------------------*
032100010330     C     INZVAR        BEGSR
032200010330     C*
032300020204     C                   Z-ADD     *zeros        Num5_0            5 0
032400020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032500020322     C                   MOVEL     '0'           FlgCAS            1
032600010330     C*
032700010330     C                   ENDSR
032800010330     C*----------------------------------------------------*
032900020722     C*  IMPOSTAZIONE CAMPI COSTANTI
033000010330     C*----------------------------------------------------*
033100010330     C     DEFCAM        BEGSR
033200010330     C*
033300020204     C* Inizializzo il buffer del record da scrivere
033400021025     C                   CLEAR                   FIVAB000
033500020204     C* Imposto i valori di default...
033600020829     C                   EVAL      VABCCM = 0530780
033700020829     C                   EVAL      VABLNP = 053
033800020204     C* ... e poi verifico se sono stati passati come parametri
033900020204     C                   IF        vlrppt > *blanks
034000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034100020204     C                   EXSR      CHKNUM
034200020204     C                   IF        PiInt=*on
034300020204     C                   Z-ADD     PiVal         VABCCM
034400020204     C                   ENDIF
034500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034600020204     C                   EXSR      CHKNUM
034700020204     C                   IF        PiInt=*on
034800020204     C                   Z-ADD     PiVal         VABLNP
034900020204     C                   ENDIF
035000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035100020204     C                   EXSR      CHKNUM
035200020204     C                   IF        PiInt=*on
035300020204     C                   Z-ADD     PiVal         VABCTR
035400020204     C                   ENDIF
035500020204     C                   ENDIF
035600020204     C*
035700010330     C                   ENDSR
035800010607     C*----------------------------------------------------*
035900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
036000010607     C*----------------------------------------------------*
036100010607     C     IMPVAB        BEGSR
036200010607     C*
036300010607     C                   SETOFF                                       3132
036400020829     C*
036500020829     C                   MOVEL     datcor        VABAAS
036600020829     C                   MOVE      datcor        VABMGS
036700010607     C*
036800010607     C* Reperimento campi ALFA
036900010607     C*
037000010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
037100020829     C                   EVAL      VABRSD=RAG
037200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
037400020117     C* ==
037500020829     C                   EVAL      VABIND=IND
037600020829     C                   EVAL      VABLOD=LOC
037700020829     C                   EVAL      VABPRD=PRV
037800020829     C                   MOVEL     CAP           Num5_0
037900020829     C                   MOVEL     Num5_0        VABCAD
038000020829     C                   MOVE      COL           VABNCL
038100020829     C                   MOVE      RIFNUM        VABRMN
038200020322     C*
038300020322     C* Considerazioni finali su CBO/CAS
038400020322     C                   IF        FlgCAS = '1'
038500020322     C                   EVAL      VABCBO = '4'
038600020322     C                   ELSE
038700020322     C                   EVAL      VABCBO = '1'
038800020322     C                   ENDIF
038900020204     C*
039000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039100020204     C                   EXSR      CHKIMPDIV
039200020204     C*
039300010607     C                   ENDSR
039400010607     C*----------------------------------------------------*
039500010601
039600020204
039700020204     C*----------------------------------------------------*
039800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
039900020204     C*----------------------------------------------------*
040000020204     C     CHKIMPDIV     BEGSR
040100020204     C*
040200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
040300020204     C                   Z-ADD     *zeros        wrkDec            9 9
040400020204     C*
040500020204     C* Come prima cosa effettuo considerazioni sulla divisa
040600020204     C                   IF        vabIAS > *zeros
040700020204     C                   IF        vabVAS <> 'EUR'
040800020204     C                   EVAL      vabVAS =  'ITL'
040900020204     C                   ENDIF
041000020204     C                   ENDIF
041100020204     C*
041200020204     C                   IF        vabCAS > *zeros
041300020204     C                   IF        vabVCA <> 'EUR'
041400020204     C                   EVAL      vabVCA =  'ITL'
041500020204     C                   ENDIF
041600020204     C                   ENDIF
041700020204     C*
041800020204     C                   IF        vabVMD > *zeros
041900020321     C                   IF        vabVAD <> 'EUR'
042000020204     C                   EVAL      vabVAD =  'ITL'
042100020204     C                   ENDIF
042200020204     C                   ENDIF
042300020204     C*
042400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
042500020204     C                   Z-ADD     vabIAS        wrkDec
042600020204     C                   IF        wrkDec > *zeros
042700020204     C                   IF        vabVAS = 'ITL'
042800020204     C                   EVAL      vabIAS = *zeros
042900020204     C                   ENDIF
043000020204     C                   ENDIF
043100020204     C*
043200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
043300020204     C                   Z-ADD     vabCAS        wrkDec
043400020204     C                   IF        wrkDec > *zeros
043500020204     C                   IF        vabVCA = 'ITL'
043600020204     C                   EVAL      vabCAS = *zeros
043700020204     C                   ENDIF
043800020204     C                   ENDIF
043900020204     C*
044000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
044100020204     C                   Z-ADD     vabVMD        wrkDec
044200020204     C                   IF        wrkDec > *zeros
044300020204     C                   IF        vabVAD = 'ITL'
044400020204     C                   EVAL      vabVMD = *zeros
044500020204     C                   ENDIF
044600020204     C                   ENDIF
044700020204     C*
044800020204     C                   ENDSR
044900020204     C***
045000020204
045100010330
045200010330
045300010330     C*----------------------------------------------------*
045400010330     C*  CONTROLLO NUMERICITA' CAMPI
045500010330     C*----------------------------------------------------*
045600010330     C     CHKNUM        BEGSR
045700010330     C*
045800010606     C                   IF        PiDecChr = *blanks
045900010606     C                   EVAL      PiDecChr = ','
046000010606     C                   ENDIF
046100010606     C*
046200010606     C                   CALL(e)   'ISNUMERIC'
046300010330     C                   PARM                    PiStr            30
046400010606     C                   PARM                    PiDecChr          1
046500010330     C                   PARM      *ZEROS        PiVal            30 9
046600010330     C                   PARM      '0'           PiInt             1
046700010330     C                   PARM      '0'           PiNum             1
046800010330     C                   IF        %error
046900010606     C                   EVAL      PiNum=*off
047000010330     C                   ENDIF
047100010330     C*
047200010330     C                   ENDSR
047300010330     C***
047400010330
047500010601
047600010601
047700010601
047800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
047900010601     C     repfil        BEGSR
048000010601     C*
048100010601     C                   if        invfil = *zeros and
048200010601     C                             depfil > *zeros and
048300010629     C                             (vinflg = *blanks or
048400010629     C                              vinflg = *zeros)
048500010601     C
048600010601     C                   eval      invfil = depfil
048700010601     C                   endif
048800010601     C*
048900010601     C                   if        depfil <> invfil and
049000010601     C                             invfil > *zeros
049100010601     C                   eval      flgMulti = '1'
049200010601     C                   if        vinflg = *blanks
049300010601     C                   add       1             cntNonEl
049400010601     C                   endif
049500010601     C                   endif
049600010601     C*
049700010601     C                   if        vinflg = '2'
049800010601     C                   eval      flgStato = '2'
049900010601     C                   endif
050000010601     C*
050100010601     C                   ENDSR
050200010601     C***
050300010601
050400010601
050500010601
050600010330
050700010330
050800010330
050900990920      /TITLE Invio dei dati al punto operativo.
051000000613     C     invio         BEGSR
051100990920     C*
051200990920     C                   reset                   dscmz
051300010601     C                   move      invfil        cmzdst
051400990920     C                   eval      cmzfld = vlrfou
051500990920     C                   eval      cmzmbd = vlrhdl
051600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
051700000710     C                   if        prmfir = *blanks
051800021025     C                   eval      cmzfla = 'FIVAB00F'
051900021025     C                   eval      cmzmba = 'FIVAB00F'
052000000710     C                   else
052100000710     C                   eval      cmzfla = prmfir
052200000710     C                   eval      cmzmba = prmfir
052300000710     C                   endif
052400990920     C                   eval      cmznrr = *zeros
052500990920     C                   move      §ctrok        cmznrr
052600021018     C                   eval      cmzlba = vlrfl1
052700990920     C                   call(e)   'TIS711C'
052800990920     C                   parm                    dscmz
052900990921     C                   parm      *blanks       esito
053000990920     C                   if        %error
053100990920     C                             or cmzerr = '1'
053200990921     C                             or esito  = '1'
053300000710     C                   eval      wrkesito = '3'
053400990920     C                   endif
053500990920     C*
053600000613     C                   ENDSR
053700990910
053800010601
053900010601
054000010601
054100010601
054200010601      /TITLE Invio dei dati al punto operativo.
054300010601     C     opeini        BEGSR
054400010601     C*
054500010601     C* Inizializzo flag e contatori operativi
054600010601     C                   movel     '0'           flgGiro           1
054700010601     C                   movel     '0'           flgMulti          1
054800010601     C                   movel     '1'           flgStato          1
054900010615     C                   movel     '0'           flgOk             1
055000010601     C                   z-add     *zeros        cntNonEl         10 0
055100010601     C                   z-add     *zeros        depfil            3 0
055200010601     C                   z-add     *zeros        invfil            3 0
055300010601     C*
055400010601     C                   ENDSR
055500010601     C***
055600010601
055700010601
055800010601
055900010330
056000010330
056100000613     C     *inzsr        BEGSR
056200990910     C*
056300990910     C     *entry        plist
056400990920     C                   parm                    tivlrds
056500990921     C                   parm      wrkesito      esito
056600000724     C                   parm                    prmlit
056700000710     C                   parm                    prmfir
056800010330     C*
056900010330     C* CALCOLA LA DATA CORRENTE
057000010330     C                   time                    wn14             14 0
057100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
057200010330     C                   z-add     wn8           g08dat
057300010330     C                   z-add     *zeros        g08inv
057400010330     C                   movel     '0'           g08err
057500010330     C                   call      'XSRDA8'
057600010330     C                   parm                    wlbda8
057700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
057800000613     C*
057900000613     C                   ENDSR
058000000613     C***
058100990908
058200020829     Otitv22p   E            riepilogo         2
058300990915     O                                              'Upload via Internet'
058400990915     O                                           +1 'Traduzione TIVIN00R -'
058500021025     O                                           +1 'FIVABWWR'
058600990915     O                                           +1 'Testate bolle'
058700990915     O          E            riepilogo   2
058800990915     O                       wrkdata
058900990915     O                       wrkora              +1
059000990915     O                       procname            +1
059100990915     O          E            riepilogo   2
059200990915     O                                              'Cliente..................:'
059300010605     O                       VABCCM        z     +1
059400990915     O          E            riepilogo   2
059500990920     O                                              'Riferimento Strategi.....:'
059600990920     O                       vlrhdl              +1
059700990915     O          E            riepilogo   2
059800990915     O                                              'Giusti...................:'
059900971022     O                       §CTROK        2   +  1
060000990915     O          E            riepilogo   2
060100990915     O                                              'Sbagliati e corretti.....:'
060200971022     O                       §CTRMO        2   +  1
060300990915     O          E            riepilogo   2
060400990915     O                                              'Sbagliati e scartati.....:'
060500971022     O                       §CTRNO        2   +  1
060600000613
060700020829     Otitv22ps  E            testdett          2
060800000613     O                                              'Upload via Internet'
060900000613     O                                           +1 'Traduzione TIVIN00R -'
061000021025     O                                           +1 'FIVABWWR'
061100000613     O                                           +1 'Testate bolle'
061200000616     O          E            testdett    3
061300000613     O                                           +2 'N° rec'
061400000613     O                                           +3 'Anteprima contenuto'
061500000616     O          E            rigadett    2
061600000613     O                       rrnum               +2
061700000621     O                       recko               +3
061800000616     O          E            findett     2
061900000613     O                       wrkdata
062000000613     O                       wrkora              +1
062100000613     O                       procname            +1
062200000616     O          E            findett     2
062300000613     O                                              'Cliente..................:'
062400010605     O                       VABCCM        z     +1
062500000616     O          E            findett     2
062600000613     O                                              'Riferimento Strategi.....:'
062700000613     O                       vlrhdl              +1
062800000616     O          E            findett     2
062900000613     O                                              'Giusti...................:'
063000000613     O                       §CTROK        2   +  1
063100000616     O          E            findett     2
063200000613     O                                              'Sbagliati e corretti.....:'
063300000613     O                       §CTRMO        2   +  1
063400000616     O          E            findett     2
063500000613     O                                              'Sbagliati e scartati.....:'
063600000613     O                       §CTRNO        2   +  1
063700000613** CMD - COMANDI CL
063800020829OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV22P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
063900020829OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV22PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
064000020829DLTOVR FILE(TITV22P TITV22PS) LVL(*)
064100010606
064200000613
