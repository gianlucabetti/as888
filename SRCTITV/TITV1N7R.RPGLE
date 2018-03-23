000100080410      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200081118     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600040301     FFIVATwwr  O    E             DISK    usropn
000700090310     Ftitv1n7p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900090310     Ftitv1n7ps O    f  198        PRINTER usropn
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
004100081118
004200081118     D*------------------
004300081118     D* LINKING A DEFINIZIONI ESTERNE
004400081118     D*------------------
004500081118     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004600081218     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004700081118
004800010330
004900010330
005000010330
005100990915     C                   time                    wrkdata
005200990915     C                   time                    wrkora
005300000913     C                   reset                   rrnum
005400990921     C                   reset                   esito
005500990921     C                   reset                   wrkesito
005600010601     C*
005700010601     C                   exsr      opeini
005800040301     C                   exsr      rwvat
005900010601     C*
006000010601     C                   seton                                        lr
006100010601
006200010601
006300010601
006400010601
006500010601     C*--------------------------------------------------------
006600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006700010601     C*--------------------------------------------------------
006800010601     C     PREELA        BEGSR
006900010601     C*
007000010601     C* SE OCCORRE SPEDIRE IN FILIALE
007100010601     C                   if        invfil <> *zeros and
007200010601     C                             flgGiro = '0'
007300010601     C*
007400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007500010601     C                   eval      flgGiro = '1'
007600000724     C*
007700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007900010601     C     invfil        chain     azorg01l
008000000724     C                   if        %found
008100000724     C                   movel(p)  CMD(1)        depcmd
008200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008300000724     C*
008400000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008500011113     C                   Z-ADD     150           LENGH            15 5
008600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008700000724     C                   PARM                    depcmd
008800000724     C                   PARM                    LENGH
008900000724     C*
009000000724     C                   endif
009100000616     C*
009200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009300000616     C                   movel(p)  CMD(2)        depcmd
009400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009500000616     C*
009600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009700011113     C                   Z-ADD     150           LENGH            15 5
009800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009900000616     C                   PARM                    depcmd
010000000616     C                   PARM                    LENGH
010100000613     C*
010200090310     C                   if        not %open(titv1n7ps)
010300090310     C                   open      titv1n7ps
010400000616     C                   except    testdett
010500000613     C                   endif
010600010601     C*
010700010601     C                   endif
010800010601     C*
010900010601     C                   ENDSR
011000010601     C***
011100010601
011200010601
011300010601
011400010601     C*--------------------------------------------------------
011500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011600010601     C*--------------------------------------------------------
011700010601     C     ENDELA        BEGSR
011800990915     C*
011900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012000000613     C*
012100090310     C                   if        %open(titv1n7ps)
012200000616     C                   except    findett
012300090310     C                   close     titv1n7ps
012400000613     C                   endif
012500000616     C*
012600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012700011113     C                   Z-ADD     150           LENGH            15 5
012800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012900000616     C                   PARM                    CMD(3)
013000000616     C                   PARM                    LENGH
013100000616     C*
013200010601     C                   ENDSR
013300010601     C***
013400010601
013500010601
013600000613
013700000613     C*--------------------------------------------------------
013800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013900000613     C*--------------------------------------------------------
014000000621     C     STPR          BEGSR
014100000613     C*
014200090310     C                   if        not %open(titv1n7p)
014300090310     C                   open      titv1n7p
014400990915     C                   endif
014500990915     C*
014600990915     C                   except    riepilogo
014700990915     C*
014800090310     C                   if        %open(titv1n7p)
014900090310     C                   close     titv1n7p
015000990914     C                   endif
015100000613     C*
015200000613     C                   ENDSR
015300000613     C***
015400000613
015500010601
015600010601
015700010330     C*--------------------------------------------------------
015800040301     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
015900010330     C*--------------------------------------------------------
016000040301     C     RWVAT         BEGSR
016100010330     C*
016200010330     C                   if        not %open(tivin00r)
016300010330     C                   open      tivin00r
016400010330     C                   endif
016500040301     C                   if        not %open(fivatwwr)
016600040301     C                   open      fivatwwr
016700010330     C                   endif
016800010330     C*
016900010604     C                   clear                   §CTROK
017000010604     C                   clear                   §CTRMO
017100010604     C                   clear                   §CTRNO
017200010330     C*
017300010330     C                   DO        *HIVAL
017400010330     C*
017500010330     C                   READ      tivin00r                               70
017600010618     C*
017700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017800010618     C                   if        vinflg = '1'
017900010618     C                   eval      flgOk = '1'
018000010618     C                   endif
018100010618     C*
018200010330     C                   if        vindta > *blanks
018300010330     C                   add       1             rrnum
018400010330     C*
018500010601     C                   if        *in70 = *off and
018600010330     C                             (vinflg = *blanks
018700010330     C                              or vinflg = '0'
018800010330     C                              or vinflg = '2')
018900010330     C*
019000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019100010711     C                   if        vinflg = *blanks or vinflg = '0'
019200010711     C                   clear                   vinmsg
019300010711     C                   endif
019400010601     C*
019500040301     C* Inizializzo campi d wrk e imposto i valori d default
019600010330     C                   exsr      inzvar
019700010330     C                   exsr      defcam
019800010601     C*
019900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020000040301     C                   eval      depfil = VATLNP
020100010601     C                   exsr      repfil
020200010601     C                   if        depfil = invfil
020300021025     C                   if        vlrpoi = 999
020400040301     C                   MOVE(P)   invfil        VATFGS
020500021025     C                   else
020600040301     C                   MOVE(P)   vlrpoi        VATFGS
020700021025     C                   endif
020800050114     C                   endif
020900040301     C*
021000040301     C* Imposto e scarico il buffer del FIVAT
021100040301     C                   exsr      impvat
021200010601     C*
021300010601     C                   exsr      PREELA
021400010601     C*
021500010604     C* Ebbene...
021600010604     C*
021700010604     C  N31              ADD       1             §CTROK            7 0
021800010604     C   32              ADD       1             §CTRMO            7 0
021900010604     C   31              ADD       1             §CTRNO            7 0
022000010604     C*
022100020722     C                   endif
022200020722     C*
022300010604     C                   if        *in31 = *off and
022400010604     C                             *in32 = *off
022500010604     C                   eval      vinflg = '1'
022600010604     C                   else
022700010604     C                   eval      recko = vindta
022800010604     C                   except    rigadett
022900010604     C                   eval      vinflg = '2'
023000010604     C                   endif
023100010604     C*
023200010330     C                   else
023300010330     C                   eval      vinflg = '1'
023400010330     C                   endif
023500010601     C*
023600010601     C  N70              update    tivin000
023700010330     C*
023800010330     C  N70              ENDdo
023900010601     C
024000010601     C*
024100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024200010601     C                   if        cntNonEl = *zeros or
024300010601     C                             flgMulti = '0'
024400010330     C* Se non ci sono record con errori ...
024500010601     C                   if        §ctrno = 0 and
024600010604     C                             §ctrmo = 0 and
024700010601     C                             flgStato <> '2'
024800010330     C* ... restituisco esito OK.
024900010330     C                   eval      wrkesito = '0'
025000010330     C                   else
025100010330     C                   if        §ctrok > 0
025200010330     C                   eval      wrkesito = '1'
025300010330     C                   else
025400010615     C                   if        flgOk = '0'
025500010615     C                   eval      wrkesito = '2'
025600010615     C                   else
025700010615     C                   eval      wrkesito = '6'
025800010615     C                   endif
025900010330     C                   endif
026000010330     C                   endif
026100010601     C                   else
026200010601     C                   eval      wrkesito = '9'
026300010601     C                   endif
026400010330     C*
026500010330     C                   if        %open(tivin00r)
026600010330     C                   close     tivin00r
026700010330     C                   endif
026800040301     C                   if        %open(fivatwwr)
026900040301     C                   close     fivatwwr
027000010330     C                   endif
027100010601     C*
027200010601     C                   if        vlrpoi <> 999
027300010601     C                   eval      invfil = vlrpoi
027400010601     C                   endif
027500010330     C*
027600010330     C                   if        §ctrok > 0
027700010601     C                             and invfil > *zeros
027800010330     C                   exsr      invio
027900010330     C                   endif
028000010601     C*
028100010618     C                   if        flgGiro = '1'
028200010601     C                   exsr      endela
028300010618     C                   endif
028400010330     C*
028500010330     C                   ENDSR
028600010330     C***
028700010330
028800010601
028900010601
029000010601
029100010601
029200010330     C*----------------------------------------------------*
029300020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029400010330     C*----------------------------------------------------*
029500010330     C     INZVAR        BEGSR
029600051219     C*
029700010330     C*
029800010330     C                   ENDSR
029900010330     C*----------------------------------------------------*
030000020722     C*  IMPOSTAZIONE CAMPI COSTANTI
030100010330     C*----------------------------------------------------*
030200010330     C     DEFCAM        BEGSR
030300010330     C*
030400020204     C* Inizializzo il buffer del record da scrivere
030500040301     C                   CLEAR                   FIVAT000
030600020204     C* Imposto i valori di default...
030700090310     C                   EVAL      VATCCM = 0961218
030800090310     C                   EVAL      VATLNP = 096
030900040301     C                   EVAL      VATTRC = 'E'
031000020204     C* ... e poi verifico se sono stati passati come parametri
031100020204     C                   IF        vlrppt > *blanks
031200040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031400020204     C                   EXSR      CHKNUM
031500020204     C                   IF        PiInt=*on
031600040301     C                   Z-ADD     PiVal         VATCCM
031700020204     C                   ENDIF
031800040301     C                   ENDIF
031900040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
032000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032100020204     C                   EXSR      CHKNUM
032200020204     C                   IF        PiInt=*on
032300040301     C                   Z-ADD     PiVal         VATLNP
032400020204     C                   ENDIF
032500040301     C                   ENDIF
032600020204     C                   ENDIF
032700020204     C*
032800010330     C                   ENDSR
032900010607     C*----------------------------------------------------*
033000040301     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
033100010607     C*----------------------------------------------------*
033200040301     C     IMPVAT        BEGSR
033300010607     C*
033400010607     C                   SETOFF                                       3132
033500040316     C*
033600040316     C* Campi "calcolati"
033700040316     C                   MOVEL     datcor        VATAAS
033800010607     C*
033900010607     C* Reperimento campi ALFA
034000090310     C                   EVAL      VATNOT = %trim(%subst(vindta:22:20))
034100040301     C*
034200040301     C* Reperimento campi NUMERICI
034300090310     C                   EVAL      PiStr=%trim(%subst(vindta:12:10))
034400040301     C                   EXSR      CHKNUM
034500040301     C                   IF        PiInt=*on
034600040301     C                   Z-ADD     PiVal         VATNSP
034700040301     C                   ELSE
034800040301     C                   SETON                                        31
034900040301     C                   Z-ADD     *zeros        VATNSP
035000040301     C                   EVAL      vinmsg = %trimr(vinmsg)
035100040301     C                             + ' ' + 'VATNSP'
035200040301     C                   ENDIF
035300040301     C*
035400040301     C* Scarico il buffer del FIVAT
035500040301     C  N31              WRITE     FIVAT000
035600020204     C*
035700010607     C                   ENDSR
035800010607     C*----------------------------------------------------*
035900020204
036000010330
036100010330
036200010330     C*----------------------------------------------------*
036300010330     C*  CONTROLLO NUMERICITA' CAMPI
036400010330     C*----------------------------------------------------*
036500010330     C     CHKNUM        BEGSR
036600081118     C*
036700081118     C                   IF        PiDecChr = *blanks
036800081118     C                   EVAL      PiDecChr = ','
036900081118     C                   ENDIF
037000081118     C*
037100081118     C                   callp(e)  UBISNUM_Check(PiStr
037200081118     C                                          :PiDecChr
037300081118     C                                          :PiVal
037400081118     C                                          :PiNum
037500081118     C                                          :PiInt)
037600081118     C*
037700081118     C                   IF        %error
037800081118     C                   EVAL      PiInt=*off
037900081118     C                   ENDIF
038000081118     C*
038100010330     C                   ENDSR
038200010330     C***
038300010330
038400010601
038500010601
038600010601
038700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
038800010601     C     repfil        BEGSR
038900010601     C*
039000010601     C                   if        invfil = *zeros and
039100010601     C                             depfil > *zeros and
039200010629     C                             (vinflg = *blanks or
039300010629     C                              vinflg = *zeros)
039400010601     C
039500010601     C                   eval      invfil = depfil
039600010601     C                   endif
039700010601     C*
039800010601     C                   if        depfil <> invfil and
039900010601     C                             invfil > *zeros
040000010601     C                   eval      flgMulti = '1'
040100010601     C                   if        vinflg = *blanks
040200010601     C                   add       1             cntNonEl
040300010601     C                   endif
040400010601     C                   endif
040500010601     C*
040600010601     C                   if        vinflg = '2'
040700010601     C                   eval      flgStato = '2'
040800010601     C                   endif
040900010601     C*
041000010601     C                   ENDSR
041100010601     C***
041200010601
041300010601
041400010601
041500010330
041600010330
041700010330
041800990920      /TITLE Invio dei dati al punto operativo.
041900000613     C     invio         BEGSR
042000990920     C*
042100990920     C                   reset                   dscmz
042200010601     C                   move      invfil        cmzdst
042300990920     C                   eval      cmzfld = vlrfou
042400990920     C                   eval      cmzmbd = vlrhdl
042500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
042600000710     C                   if        prmfir = *blanks
042700040301     C                   eval      cmzfla = 'FIVAT00F'
042800040301     C                   eval      cmzmba = 'FIVAT00F'
042900000710     C                   else
043000000710     C                   eval      cmzfla = prmfir
043100000710     C                   eval      cmzmba = prmfir
043200000710     C                   endif
043300990920     C                   eval      cmznrr = *zeros
043400990920     C                   move      §ctrok        cmznrr
043500021018     C                   eval      cmzlba = vlrfl1
043600990920     C                   call(e)   'TIS711C'
043700990920     C                   parm                    dscmz
043800990921     C                   parm      *blanks       esito
043900990920     C                   if        %error
044000990920     C                             or cmzerr = '1'
044100990921     C                             or esito  = '1'
044200000710     C                   eval      wrkesito = '3'
044300990920     C                   endif
044400990920     C*
044500000613     C                   ENDSR
044600990910
044700010601
044800010601
044900010601
045000010601
045100010601      /TITLE Invio dei dati al punto operativo.
045200010601     C     opeini        BEGSR
045300010601     C*
045400010601     C* Inizializzo flag e contatori operativi
045500010601     C                   movel     '0'           flgGiro           1
045600010601     C                   movel     '0'           flgMulti          1
045700010601     C                   movel     '1'           flgStato          1
045800010615     C                   movel     '0'           flgOk             1
045900010601     C                   z-add     *zeros        cntNonEl         10 0
046000010601     C                   z-add     *zeros        depfil            3 0
046100010601     C                   z-add     *zeros        invfil            3 0
046200010601     C*
046300010601     C                   ENDSR
046400010601     C***
046500010601
046600010601
046700010601
046800010330
046900010330
047000000613     C     *inzsr        BEGSR
047100990910     C*
047200990910     C     *entry        plist
047300990920     C                   parm                    tivlrds
047400990921     C                   parm      wrkesito      esito
047500000724     C                   parm                    prmlit
047600000710     C                   parm                    prmfir
047700010330     C*
047800010330     C* CALCOLA LA DATA CORRENTE
047900010330     C                   time                    wn14             14 0
048000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
048100010330     C                   z-add     wn8           g08dat
048200010330     C                   z-add     *zeros        g08inv
048300010330     C                   movel     '0'           g08err
048400010330     C                   call      'XSRDA8'
048500010330     C                   parm                    wlbda8
048600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
048700000613     C*
048800000613     C                   ENDSR
048900000613     C***
049000990908
049100090310     Otitv1n7p  E            riepilogo         2
049200990915     O                                              'Upload via Internet'
049300990915     O                                           +1 'Traduzione TIVIN00R -'
049400040301     O                                           +1 'FIVATWWR'
049500040301     O                                           +1 'Dettaglio barcode'
049600990915     O          E            riepilogo   2
049700990915     O                       wrkdata
049800990915     O                       wrkora              +1
049900990915     O                       procname            +1
050000990915     O          E            riepilogo   2
050100990915     O                                              'Cliente..................:'
050200040301     O                       VATCCM        z     +1
050300990915     O          E            riepilogo   2
050400990920     O                                              'Riferimento Strategi.....:'
050500990920     O                       vlrhdl              +1
050600990915     O          E            riepilogo   2
050700990915     O                                              'Giusti...................:'
050800971022     O                       §CTROK        2   +  1
050900990915     O          E            riepilogo   2
051000990915     O                                              'Sbagliati e corretti.....:'
051100971022     O                       §CTRMO        2   +  1
051200990915     O          E            riepilogo   2
051300990915     O                                              'Sbagliati e scartati.....:'
051400971022     O                       §CTRNO        2   +  1
051500000613
051600090310     Otitv1n7ps E            testdett          2
051700000613     O                                              'Upload via Internet'
051800000613     O                                           +1 'Traduzione TIVIN00R -'
051900040301     O                                           +1 'FIVATWWR'
052000040301     O                                           +1 'Dettaglio barcode'
052100000616     O          E            testdett    3
052200000613     O                                           +2 'N° rec'
052300000613     O                                           +3 'Anteprima contenuto'
052400000616     O          E            rigadett    2
052500000613     O                       rrnum               +2
052600000621     O                       recko               +3
052700000616     O          E            findett     2
052800000613     O                       wrkdata
052900000613     O                       wrkora              +1
053000000613     O                       procname            +1
053100000616     O          E            findett     2
053200000613     O                                              'Cliente..................:'
053300040301     O                       VATCCM        z     +1
053400000616     O          E            findett     2
053500000613     O                                              'Riferimento Strategi.....:'
053600000613     O                       vlrhdl              +1
053700000616     O          E            findett     2
053800000613     O                                              'Giusti...................:'
053900000613     O                       §CTROK        2   +  1
054000000616     O          E            findett     2
054100000613     O                                              'Sbagliati e corretti.....:'
054200000613     O                       §CTRMO        2   +  1
054300000616     O          E            findett     2
054400000613     O                                              'Sbagliati e scartati.....:'
054500000613     O                       §CTRNO        2   +  1
054600000613** CMD - COMANDI CL
054700090310OVRPRTF OVRSCOPE(*CALLLVL) FILE(TITV1N7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
054800090310OVRPRTF OVRSCOPE(*CALLLVL) FILE(TITV1N7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
054900090310DLTOVR FILE(TITV1N7P TITV1N7PS) LVL(*)
055000010606
055100000613
