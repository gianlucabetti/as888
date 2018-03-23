000100081016      /TITLE Upload via Internet: traduzione in EDIVATWR.
000200130114     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600081016     FEDIVATwr  O    E             DISK    usropn
000700081007     Ftitv1j2p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900081007     Ftitv1j2ps O    f  198        PRINTER usropn
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
004000081007     D*------------------
004100081007     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
004200081007     D*------------------
004300081007     D PiStr           S            100A   INZ
004400081007     D PiDecChr        S              1A   INZ
004500081007     D PiVal           S             63S30 INZ
004600081007     D PiInt           S               N   INZ
004700081007     D PiNum           S               N   INZ
004800081007
004900081007     D*------------------
005000081007     D* LINKING A DEFINIZIONI ESTERNE
005100081007     D*------------------
005200081007     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005300081007
005400081007
005500010330
005600990915     C                   time                    wrkdata
005700990915     C                   time                    wrkora
005800000913     C                   reset                   rrnum
005900990921     C                   reset                   esito
006000990921     C                   reset                   wrkesito
006100010601     C*
006200010601     C                   exsr      opeini
006300040301     C                   exsr      rwvat
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
010700081007     C                   if        not %open(titv1j2ps)
010800081007     C                   open      titv1j2ps
010900130114     C***                except    testdett
011000000613     C                   endif
011100010601     C*
011200010601     C                   endif
011300010601     C*
011400010601     C                   ENDSR
011500010601     C***
011600010601
011700010601
011800010601
011900010601     C*--------------------------------------------------------
012000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012100010601     C*--------------------------------------------------------
012200010601     C     ENDELA        BEGSR
012300990915     C*
012400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012500000613     C*
012600081007     C                   if        %open(titv1j2ps)
012700130114     C***                except    findett
012800081007     C                   close     titv1j2ps
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
014100000613
014200000613     C*--------------------------------------------------------
014300000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014400000613     C*--------------------------------------------------------
014500000621     C     STPR          BEGSR
014600000613     C*
014700081007     C                   if        not %open(titv1j2p)
014800081007     C                   open      titv1j2p
014900990915     C                   endif
015000990915     C*
015100130114     C***                except    riepilogo
015200990915     C*
015300081007     C                   if        %open(titv1j2p)
015400081007     C                   close     titv1j2p
015500990914     C                   endif
015600000613     C*
015700000613     C                   ENDSR
015800000613     C***
015900000613
016000010601
016100010601
016200010330     C*--------------------------------------------------------
016300081016     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
016400010330     C*--------------------------------------------------------
016500040301     C     RWVAT         BEGSR
016600010330     C*
016700010330     C                   if        not %open(tivin00r)
016800010330     C                   open      tivin00r
016900010330     C                   endif
017000081016     C                   if        not %open(edivatwr)
017100081016     C                   open      edivatwr
017200010330     C                   endif
017300010330     C*
017400010604     C                   clear                   §CTROK
017500010604     C                   clear                   §CTRMO
017600010604     C                   clear                   §CTRNO
017700010330     C*
017800010330     C                   DO        *HIVAL
017900010330     C*
018000010330     C                   READ      tivin00r                               70
018100010618     C*
018200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018300010618     C                   if        vinflg = '1'
018400010618     C                   eval      flgOk = '1'
018500010618     C                   endif
018600010618     C*
018700010330     C                   if        vindta > *blanks
018800010330     C                   add       1             rrnum
018900010330     C*
019000010601     C                   if        *in70 = *off and
019100010330     C                             (vinflg = *blanks
019200010330     C                              or vinflg = '0'
019300010330     C                              or vinflg = '2')
019400010330     C*
019500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019600010711     C                   if        vinflg = *blanks or vinflg = '0'
019700010711     C                   clear                   vinmsg
019800010711     C                   endif
019900010601     C*
020000040301     C* Inizializzo campi d wrk e imposto i valori d default
020100010330     C                   exsr      inzvar
020200010330     C                   exsr      defcam
020300010601     C*
020400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020500040301     C                   eval      depfil = VATLNP
020600010601     C                   exsr      repfil
020700010601     C                   if        depfil = invfil
020800021025     C                   if        vlrpoi = 999
020900040301     C                   MOVE(P)   invfil        VATFGS
021000021025     C                   else
021100040301     C                   MOVE(P)   vlrpoi        VATFGS
021200021025     C                   endif
021300050114     C                   endif
021400040301     C*
021500081016     C* Imposto e scarico il buffer del EDIVAT
021600040301     C                   exsr      impvat
021700010601     C*
021800010601     C                   exsr      PREELA
021900010601     C*
022000010604     C* Ebbene...
022100010604     C*
022200010604     C  N31              ADD       1             §CTROK            7 0
022300010604     C   32              ADD       1             §CTRMO            7 0
022400010604     C   31              ADD       1             §CTRNO            7 0
022500010604     C*
022600020722     C                   endif
022700020722     C*
022800010604     C                   if        *in31 = *off and
022900010604     C                             *in32 = *off
023000010604     C                   eval      vinflg = '1'
023100010604     C                   else
023200010604     C                   eval      recko = vindta
023300130114     C***                except    rigadett
023400010604     C                   eval      vinflg = '2'
023500010604     C                   endif
023600010604     C*
023700010330     C                   else
023800010330     C                   eval      vinflg = '1'
023900010330     C                   endif
024000010601     C*
024100010601     C  N70              update    tivin000
024200010330     C*
024300010330     C  N70              ENDdo
024400010601     C
024500010601     C*
024600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024700010601     C                   if        cntNonEl = *zeros or
024800010601     C                             flgMulti = '0'
024900010330     C* Se non ci sono record con errori ...
025000010601     C                   if        §ctrno = 0 and
025100010604     C                             §ctrmo = 0 and
025200010601     C                             flgStato <> '2'
025300010330     C* ... restituisco esito OK.
025400010330     C                   eval      wrkesito = '0'
025500010330     C                   else
025600010330     C                   if        §ctrok > 0
025700010330     C                   eval      wrkesito = '1'
025800010330     C                   else
025900010615     C                   if        flgOk = '0'
026000010615     C                   eval      wrkesito = '2'
026100010615     C                   else
026200010615     C                   eval      wrkesito = '6'
026300010615     C                   endif
026400010330     C                   endif
026500010330     C                   endif
026600010601     C                   else
026700010601     C                   eval      wrkesito = '9'
026800010601     C                   endif
026900010330     C*
027000010330     C                   if        %open(tivin00r)
027100010330     C                   close     tivin00r
027200010330     C                   endif
027300081016     C                   if        %open(edivatwr)
027400081016     C                   close     edivatwr
027500010330     C                   endif
027600010601     C*
027700010601     C                   if        vlrpoi <> 999
027800010601     C                   eval      invfil = vlrpoi
027900010601     C                   endif
028000010330     C*
028100010330     C                   if        §ctrok > 0
028200010601     C                             and invfil > *zeros
028300010330     C                   exsr      invio
028400010330     C                   endif
028500010601     C*
028600010618     C                   if        flgGiro = '1'
028700010601     C                   exsr      endela
028800010618     C                   endif
028900010330     C*
029000010330     C                   ENDSR
029100010330     C***
029200010330
029300010601
029400010601
029500010601
029600010601
029700010330     C*----------------------------------------------------*
029800020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029900010330     C*----------------------------------------------------*
030000010330     C     INZVAR        BEGSR
030100051219     C*
030200010330     C*
030300010330     C                   ENDSR
030400010330     C*----------------------------------------------------*
030500020722     C*  IMPOSTAZIONE CAMPI COSTANTI
030600010330     C*----------------------------------------------------*
030700010330     C     DEFCAM        BEGSR
030800010330     C*
030900020204     C* Inizializzo il buffer del record da scrivere
031000081016     C                   CLEAR                   EDIVAT00
031100020204     C* Imposto i valori di default...
031200081007     C                   EVAL      VATCCM = 2220929
031300081007     C                   EVAL      VATLNP = 222
031400040301     C                   EVAL      VATTRC = 'E'
031500020204     C* ... e poi verifico se sono stati passati come parametri
031600020204     C                   IF        vlrppt > *blanks
031700040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031900020204     C                   EXSR      CHKNUM
032000020204     C                   IF        PiInt=*on
032100040301     C                   Z-ADD     PiVal         VATCCM
032200020204     C                   ENDIF
032300040301     C                   ENDIF
032400040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
032500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032600020204     C                   EXSR      CHKNUM
032700020204     C                   IF        PiInt=*on
032800040301     C                   Z-ADD     PiVal         VATLNP
032900020204     C                   ENDIF
033000040301     C                   ENDIF
033100020204     C                   ENDIF
033200020204     C*
033300010330     C                   ENDSR
033400010607     C*----------------------------------------------------*
033500081016     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
033600010607     C*----------------------------------------------------*
033700040301     C     IMPVAT        BEGSR
033800010607     C*
033900010607     C                   SETOFF                                       3132
034000040316     C*
034100040316     C* Campi "calcolati"
034200040316     C                   MOVEL     datcor        VATAAS
034300010607     C*
034400010607     C* Reperimento campi ALFA
034500100413     C                   EVAL      VATNOT='00'+%trim(%subst(vindta:30:18))
034600040301     C*
034700040301     C* Reperimento campi NUMERICI
034800081007     C                   EVAL      PiStr=%trim(%subst(vindta:11:10))
034900040301     C                   EXSR      CHKNUM
035000040301     C                   IF        PiInt=*on
035100040301     C                   Z-ADD     PiVal         VATNSP
035200040301     C                   ELSE
035300040301     C                   SETON                                        31
035400040301     C                   Z-ADD     *zeros        VATNSP
035500040301     C                   EVAL      vinmsg = %trimr(vinmsg)
035600040301     C                             + ' ' + 'VATNSP'
035700040301     C                   ENDIF
035800081016     C*
035900081016     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
036000081016     C                   EVAL      VATCMR = %char(VATCCM)
036100081016     C                   EVAL      VATCNT = 1
036200040301     C*
036300081016     C* Scarico il buffer del EDIVAT
036400081016     C  N31              WRITE     EDIVAT00
036500020204     C*
036600010607     C                   ENDSR
036700010607     C*----------------------------------------------------*
036800020204
036900010330
037000010330
037100010330     C*----------------------------------------------------*
037200010330     C*  CONTROLLO NUMERICITA' CAMPI
037300010330     C*----------------------------------------------------*
037400010330     C     CHKNUM        BEGSR
037500081007     C*
037600081007     C                   IF        PiDecChr = *blanks
037700081007     C                   EVAL      PiDecChr = ','
037800081007     C                   ENDIF
037900081007     C*
038000081007     C                   callp     UBISNUM_Check(PiStr
038100081007     C                                          :PiDecChr
038200081007     C                                          :PiVal
038300081007     C                                          :PiNum
038400081007     C                                          :PiInt)
038500010330     C*
038600010330     C                   ENDSR
038700010330     C***
038800010330
038900010601
039000010601
039100010601
039200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
039300010601     C     repfil        BEGSR
039400010601     C*
039500010601     C                   if        invfil = *zeros and
039600010601     C                             depfil > *zeros and
039700010629     C                             (vinflg = *blanks or
039800010629     C                              vinflg = *zeros)
039900010601     C
040000010601     C                   eval      invfil = depfil
040100010601     C                   endif
040200010601     C*
040300010601     C                   if        depfil <> invfil and
040400010601     C                             invfil > *zeros
040500010601     C                   eval      flgMulti = '1'
040600010601     C                   if        vinflg = *blanks
040700010601     C                   add       1             cntNonEl
040800010601     C                   endif
040900010601     C                   endif
041000010601     C*
041100010601     C                   if        vinflg = '2'
041200010601     C                   eval      flgStato = '2'
041300010601     C                   endif
041400010601     C*
041500010601     C                   ENDSR
041600010601     C***
041700010601
041800010601
041900010601
042000010330
042100010330
042200010330
042300990920      /TITLE Invio dei dati al punto operativo.
042400000613     C     invio         BEGSR
042500990920     C*
042600990920     C                   reset                   dscmz
042700010601     C                   move      invfil        cmzdst
042800990920     C                   eval      cmzfld = vlrfou
042900990920     C                   eval      cmzmbd = vlrhdl
043000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
043100000710     C                   if        prmfir = *blanks
043200081016     C                   eval      cmzfla = 'EDIVAT0F'
043300081016     C                   eval      cmzmba = 'EDIVAT0F'
043400000710     C                   else
043500000710     C                   eval      cmzfla = prmfir
043600000710     C                   eval      cmzmba = prmfir
043700000710     C                   endif
043800990920     C                   eval      cmznrr = *zeros
043900990920     C                   move      §ctrok        cmznrr
044000021018     C                   eval      cmzlba = vlrfl1
044100990920     C                   call(e)   'TIS711C'
044200990920     C                   parm                    dscmz
044300990921     C                   parm      *blanks       esito
044400990920     C                   if        %error
044500990920     C                             or cmzerr = '1'
044600990921     C                             or esito  = '1'
044700000710     C                   eval      wrkesito = '3'
044800990920     C                   endif
044900990920     C*
045000000613     C                   ENDSR
045100990910
045200010601
045300010601
045400010601
045500010601
045600010601      /TITLE Invio dei dati al punto operativo.
045700010601     C     opeini        BEGSR
045800010601     C*
045900010601     C* Inizializzo flag e contatori operativi
046000010601     C                   movel     '0'           flgGiro           1
046100010601     C                   movel     '0'           flgMulti          1
046200010601     C                   movel     '1'           flgStato          1
046300010615     C                   movel     '0'           flgOk             1
046400010601     C                   z-add     *zeros        cntNonEl         10 0
046500010601     C                   z-add     *zeros        depfil            3 0
046600010601     C                   z-add     *zeros        invfil            3 0
046700010601     C*
046800010601     C                   ENDSR
046900010601     C***
047000010601
047100010601
047200010601
047300010330
047400010330
047500000613     C     *inzsr        BEGSR
047600990910     C*
047700990910     C     *entry        plist
047800990920     C                   parm                    tivlrds
047900990921     C                   parm      wrkesito      esito
048000000724     C                   parm                    prmlit
048100000710     C                   parm                    prmfir
048200010330     C*
048300010330     C* CALCOLA LA DATA CORRENTE
048400010330     C                   time                    wn14             14 0
048500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
048600010330     C                   z-add     wn8           g08dat
048700010330     C                   z-add     *zeros        g08inv
048800010330     C                   movel     '0'           g08err
048900010330     C                   call      'XSRDA8'
049000010330     C                   parm                    wlbda8
049100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
049200000613     C*
049300000613     C                   ENDSR
049400000613     C***
049500990908
049600081007     Otitv1j2p  E            riepilogo         2
049700990915     O                                              'Upload via Internet'
049800990915     O                                           +1 'Traduzione TIVIN00R -'
049900081016     O                                           +1 'EDIVATWR'
050000040301     O                                           +1 'Dettaglio barcode'
050100990915     O          E            riepilogo   2
050200990915     O                       wrkdata
050300990915     O                       wrkora              +1
050400990915     O                       procname            +1
050500990915     O          E            riepilogo   2
050600990915     O                                              'Cliente..................:'
050700040301     O                       VATCCM        z     +1
050800990915     O          E            riepilogo   2
050900990920     O                                              'Riferimento Strategi.....:'
051000990920     O                       vlrhdl              +1
051100990915     O          E            riepilogo   2
051200990915     O                                              'Giusti...................:'
051300971022     O                       §CTROK        2   +  1
051400990915     O          E            riepilogo   2
051500990915     O                                              'Sbagliati e corretti.....:'
051600971022     O                       §CTRMO        2   +  1
051700990915     O          E            riepilogo   2
051800990915     O                                              'Sbagliati e scartati.....:'
051900971022     O                       §CTRNO        2   +  1
052000000613
052100081007     Otitv1j2ps E            testdett          2
052200000613     O                                              'Upload via Internet'
052300000613     O                                           +1 'Traduzione TIVIN00R -'
052400081016     O                                           +1 'EDIVATWR'
052500040301     O                                           +1 'Dettaglio barcode'
052600000616     O          E            testdett    3
052700000613     O                                           +2 'N° rec'
052800000613     O                                           +3 'Anteprima contenuto'
052900000616     O          E            rigadett    2
053000000613     O                       rrnum               +2
053100000621     O                       recko               +3
053200000616     O          E            findett     2
053300000613     O                       wrkdata
053400000613     O                       wrkora              +1
053500000613     O                       procname            +1
053600000616     O          E            findett     2
053700000613     O                                              'Cliente..................:'
053800040301     O                       VATCCM        z     +1
053900000616     O          E            findett     2
054000000613     O                                              'Riferimento Strategi.....:'
054100000613     O                       vlrhdl              +1
054200000616     O          E            findett     2
054300000613     O                                              'Giusti...................:'
054400000613     O                       §CTROK        2   +  1
054500000616     O          E            findett     2
054600000613     O                                              'Sbagliati e corretti.....:'
054700000613     O                       §CTRMO        2   +  1
054800000616     O          E            findett     2
054900000613     O                                              'Sbagliati e scartati.....:'
055000000613     O                       §CTRNO        2   +  1
055100000613** CMD - COMANDI CL
055200081007OVRPRTF OVRSCOPE(*CALLLVL) FILE(TITV1J2P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
055300081007OVRPRTF OVRSCOPE(*CALLLVL) FILE(TITV1J2PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
055400081007DLTOVR FILE(TITV1J2P TITV1J2PS) LVL(*)
055500010606
055600000613
