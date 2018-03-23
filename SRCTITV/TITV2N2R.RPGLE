000100071029      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600040301     FFIVATwwr  O    E             DISK    usropn
000700120116     Ftitv2j6p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900120116     Ftitv2j6ps O    f  198        PRINTER usropn
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
005100040301     C                   exsr      rwvat
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
009500120116     C                   if        not %open(titv2j6ps)
009600120116     C                   open      titv2j6ps
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
011400120116     C                   if        %open(titv2j6ps)
011500000616     C                   except    findett
011600120116     C                   close     titv2j6ps
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
013500120116     C                   if        not %open(titv2j6p)
013600120116     C                   open      titv2j6p
013700990915     C                   endif
013800990915     C*
013900990915     C                   except    riepilogo
014000990915     C*
014100120116     C                   if        %open(titv2j6p)
014200120116     C                   close     titv2j6p
014300990914     C                   endif
014400000613     C*
014500000613     C                   ENDSR
014600000613     C***
014700000613
014800010601
014900010601
015000010330     C*--------------------------------------------------------
015100040301     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
015200010330     C*--------------------------------------------------------
015300040301     C     RWVAT         BEGSR
015400010330     C*
015500010330     C                   if        not %open(tivin00r)
015600010330     C                   open      tivin00r
015700010330     C                   endif
015800040301     C                   if        not %open(fivatwwr)
015900040301     C                   open      fivatwwr
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
018800040301     C* Inizializzo campi d wrk e imposto i valori d default
018900010330     C                   exsr      inzvar
019000010330     C                   exsr      defcam
019100010601     C*
019200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019300040301     C                   eval      depfil = VATLNP
019400010601     C                   exsr      repfil
019500010601     C                   if        depfil = invfil
019600021025     C                   if        vlrpoi = 999
019700040301     C                   MOVE(P)   invfil        VATFGS
019800021025     C                   else
019900040301     C                   MOVE(P)   vlrpoi        VATFGS
020000021025     C                   endif
020100050114     C                   endif
020200040301     C*
020300040301     C* Imposto e scarico il buffer del FIVAT
020400040301     C                   exsr      impvat
020500010601     C*
020600010601     C                   exsr      PREELA
020700010601     C*
020800010604     C* Ebbene...
020900010604     C*
021000010604     C  N31              ADD       1             §CTROK            7 0
021100010604     C   32              ADD       1             §CTRMO            7 0
021200010604     C   31              ADD       1             §CTRNO            7 0
021300010604     C*
021400020722     C                   endif
021500020722     C*
021600010604     C                   if        *in31 = *off and
021700010604     C                             *in32 = *off
021800010604     C                   eval      vinflg = '1'
021900010604     C                   else
022000010604     C                   eval      recko = vindta
022100010604     C                   except    rigadett
022200010604     C                   eval      vinflg = '2'
022300010604     C                   endif
022400010604     C*
022500010330     C                   else
022600010330     C                   eval      vinflg = '1'
022700010330     C                   endif
022800010601     C*
022900010601     C  N70              update    tivin000
023000010330     C*
023100010330     C  N70              ENDdo
023200010601     C
023300010601     C*
023400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023500010601     C                   if        cntNonEl = *zeros or
023600010601     C                             flgMulti = '0'
023700010330     C* Se non ci sono record con errori ...
023800010601     C                   if        §ctrno = 0 and
023900010604     C                             §ctrmo = 0 and
024000010601     C                             flgStato <> '2'
024100010330     C* ... restituisco esito OK.
024200010330     C                   eval      wrkesito = '0'
024300010330     C                   else
024400010330     C                   if        §ctrok > 0
024500010330     C                   eval      wrkesito = '1'
024600010330     C                   else
024700010615     C                   if        flgOk = '0'
024800010615     C                   eval      wrkesito = '2'
024900010615     C                   else
025000010615     C                   eval      wrkesito = '6'
025100010615     C                   endif
025200010330     C                   endif
025300010330     C                   endif
025400010601     C                   else
025500010601     C                   eval      wrkesito = '9'
025600010601     C                   endif
025700010330     C*
025800010330     C                   if        %open(tivin00r)
025900010330     C                   close     tivin00r
026000010330     C                   endif
026100040301     C                   if        %open(fivatwwr)
026200040301     C                   close     fivatwwr
026300010330     C                   endif
026400010601     C*
026500010601     C                   if        vlrpoi <> 999
026600010601     C                   eval      invfil = vlrpoi
026700010601     C                   endif
026800010330     C*
026900010330     C                   if        §ctrok > 0
027000010601     C                             and invfil > *zeros
027100010330     C                   exsr      invio
027200010330     C                   endif
027300010601     C*
027400010618     C                   if        flgGiro = '1'
027500010601     C                   exsr      endela
027600010618     C                   endif
027700010330     C*
027800010330     C                   ENDSR
027900010330     C***
028000010330
028100010601
028200010601
028300010601
028400010601
028500010330     C*----------------------------------------------------*
028600020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028700010330     C*----------------------------------------------------*
028800010330     C     INZVAR        BEGSR
028900051219     C*
029000010330     C*
029100010330     C                   ENDSR
029200010330     C*----------------------------------------------------*
029300020722     C*  IMPOSTAZIONE CAMPI COSTANTI
029400010330     C*----------------------------------------------------*
029500010330     C     DEFCAM        BEGSR
029600010330     C*
029700020204     C* Inizializzo il buffer del record da scrivere
029800040301     C                   CLEAR                   FIVAT000
029900020204     C* Imposto i valori di default...
030000121123     C                   EVAL      VATCCM = 2230099
030100121123     C                   EVAL      VATLNP = 223
030200040301     C                   EVAL      VATTRC = 'E'
030300020204     C* ... e poi verifico se sono stati passati come parametri
030400020204     C                   IF        vlrppt > *blanks
030500040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
030600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030700020204     C                   EXSR      CHKNUM
030800020204     C                   IF        PiInt=*on
030900040301     C                   Z-ADD     PiVal         VATCCM
031000020204     C                   ENDIF
031100040301     C                   ENDIF
031200040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
031300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031400020204     C                   EXSR      CHKNUM
031500020204     C                   IF        PiInt=*on
031600040301     C                   Z-ADD     PiVal         VATLNP
031700020204     C                   ENDIF
031800040301     C                   ENDIF
031900020204     C                   ENDIF
032000020204     C*
032100010330     C                   ENDSR
032200010607     C*----------------------------------------------------*
032300040301     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
032400010607     C*----------------------------------------------------*
032500040301     C     IMPVAT        BEGSR
032600010607     C*
032700010607     C                   SETOFF                                       3132
032800040316     C*
032900040316     C* Campi "calcolati"
033000040316     C                   MOVEL     datcor        VATAAS
033100010607     C*
033200010607     C* Reperimento campi ALFA
033300080311     C                   IF        %trim(%subst(vindta:97:15)) = *blanks
033400080311     C                   EVAL      VATNOT = %trim(%subst(vindta:21:15))
033500080311     C                   ELSE
033600080311     C                   EVAL      VATNOT = %trim(%subst(vindta:21:15))+
033700080311     C                                      %trim(%subst(vindta:97:15))
033800080311     C                   ENDIF
033900040301     C*
034000040301     C* Reperimento campi NUMERICI
034100080318     C* NSP
034200080311     C                   EVAL      PiStr=%trim(%subst(vindta:1:15))
034300040301     C                   EXSR      CHKNUM
034400040301     C                   IF        PiInt=*on
034500040301     C                   Z-ADD     PiVal         VATNSP
034600040301     C                   ELSE
034700040301     C                   SETON                                        31
034800040301     C                   Z-ADD     *zeros        VATNSP
034900040301     C                   EVAL      vinmsg = %trimr(vinmsg)
035000040301     C                             + ' ' + 'VATNSP'
035100040301     C                   ENDIF
035200040301     C*
035300040301     C* Scarico il buffer del FIVAT
035400040301     C  N31              WRITE     FIVAT000
035500020204     C*
035600010607     C                   ENDSR
035700010607     C*----------------------------------------------------*
035800020204
035900010330
036000010330
036100010330     C*----------------------------------------------------*
036200010330     C*  CONTROLLO NUMERICITA' CAMPI
036300010330     C*----------------------------------------------------*
036400010330     C     CHKNUM        BEGSR
036500010330     C*
036600010606     C                   IF        PiDecChr = *blanks
036700080311     C                   EVAL      PiDecChr = '.'
036800010606     C                   ENDIF
036900010606     C*
037000010606     C                   CALL(e)   'ISNUMERIC'
037100010330     C                   PARM                    PiStr            30
037200010606     C                   PARM                    PiDecChr          1
037300010330     C                   PARM      *ZEROS        PiVal            30 9
037400010330     C                   PARM      '0'           PiInt             1
037500010330     C                   PARM      '0'           PiNum             1
037600010330     C                   IF        %error
037700010606     C                   EVAL      PiNum=*off
037800010330     C                   ENDIF
037900010330     C*
038000010330     C                   ENDSR
038100010330     C***
038200010330
038300010601
038400010601
038500010601
038600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
038700010601     C     repfil        BEGSR
038800010601     C*
038900010601     C                   if        invfil = *zeros and
039000010601     C                             depfil > *zeros and
039100010629     C                             (vinflg = *blanks or
039200010629     C                              vinflg = *zeros)
039300010601     C
039400010601     C                   eval      invfil = depfil
039500010601     C                   endif
039600010601     C*
039700010601     C                   if        depfil <> invfil and
039800010601     C                             invfil > *zeros
039900010601     C                   eval      flgMulti = '1'
040000010601     C                   if        vinflg = *blanks
040100010601     C                   add       1             cntNonEl
040200010601     C                   endif
040300010601     C                   endif
040400010601     C*
040500010601     C                   if        vinflg = '2'
040600010601     C                   eval      flgStato = '2'
040700010601     C                   endif
040800010601     C*
040900010601     C                   ENDSR
041000010601     C***
041100010601
041200010601
041300010601
041400010330
041500010330
041600010330
041700990920      /TITLE Invio dei dati al punto operativo.
041800000613     C     invio         BEGSR
041900990920     C*
042000990920     C                   reset                   dscmz
042100010601     C                   move      invfil        cmzdst
042200990920     C                   eval      cmzfld = vlrfou
042300990920     C                   eval      cmzmbd = vlrhdl
042400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
042500000710     C                   if        prmfir = *blanks
042600040301     C                   eval      cmzfla = 'FIVAT00F'
042700040301     C                   eval      cmzmba = 'FIVAT00F'
042800000710     C                   else
042900000710     C                   eval      cmzfla = prmfir
043000000710     C                   eval      cmzmba = prmfir
043100000710     C                   endif
043200990920     C                   eval      cmznrr = *zeros
043300990920     C                   move      §ctrok        cmznrr
043400021018     C                   eval      cmzlba = vlrfl1
043500990920     C                   call(e)   'TIS711C'
043600990920     C                   parm                    dscmz
043700990921     C                   parm      *blanks       esito
043800990920     C                   if        %error
043900990920     C                             or cmzerr = '1'
044000990921     C                             or esito  = '1'
044100000710     C                   eval      wrkesito = '3'
044200990920     C                   endif
044300990920     C*
044400000613     C                   ENDSR
044500990910
044600010601
044700010601
044800010601
044900010601
045000010601      /TITLE Invio dei dati al punto operativo.
045100010601     C     opeini        BEGSR
045200010601     C*
045300010601     C* Inizializzo flag e contatori operativi
045400010601     C                   movel     '0'           flgGiro           1
045500010601     C                   movel     '0'           flgMulti          1
045600010601     C                   movel     '1'           flgStato          1
045700010615     C                   movel     '0'           flgOk             1
045800010601     C                   z-add     *zeros        cntNonEl         10 0
045900010601     C                   z-add     *zeros        depfil            3 0
046000010601     C                   z-add     *zeros        invfil            3 0
046100010601     C*
046200010601     C                   ENDSR
046300010601     C***
046400010601
046500010601
046600010601
046700010330
046800010330
046900000613     C     *inzsr        BEGSR
047000990910     C*
047100990910     C     *entry        plist
047200990920     C                   parm                    tivlrds
047300990921     C                   parm      wrkesito      esito
047400000724     C                   parm                    prmlit
047500000710     C                   parm                    prmfir
047600010330     C*
047700010330     C* CALCOLA LA DATA CORRENTE
047800010330     C                   time                    wn14             14 0
047900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
048000010330     C                   z-add     wn8           g08dat
048100010330     C                   z-add     *zeros        g08inv
048200010330     C                   movel     '0'           g08err
048300010330     C                   call      'XSRDA8'
048400010330     C                   parm                    wlbda8
048500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
048600000613     C*
048700000613     C                   ENDSR
048800000613     C***
048900990908
049000120116     Otitv2j6p  E            riepilogo         2
049100990915     O                                              'Upload via Internet'
049200990915     O                                           +1 'Traduzione TIVIN00R -'
049300040301     O                                           +1 'FIVATWWR'
049400040301     O                                           +1 'Dettaglio barcode'
049500990915     O          E            riepilogo   2
049600990915     O                       wrkdata
049700990915     O                       wrkora              +1
049800990915     O                       procname            +1
049900990915     O          E            riepilogo   2
050000990915     O                                              'Cliente..................:'
050100040301     O                       VATCCM        z     +1
050200990915     O          E            riepilogo   2
050300990920     O                                              'Riferimento Strategi.....:'
050400990920     O                       vlrhdl              +1
050500990915     O          E            riepilogo   2
050600990915     O                                              'Giusti...................:'
050700971022     O                       §CTROK        2   +  1
050800990915     O          E            riepilogo   2
050900990915     O                                              'Sbagliati e corretti.....:'
051000971022     O                       §CTRMO        2   +  1
051100990915     O          E            riepilogo   2
051200990915     O                                              'Sbagliati e scartati.....:'
051300971022     O                       §CTRNO        2   +  1
051400000613
051500120116     Otitv2j6ps E            testdett          2
051600000613     O                                              'Upload via Internet'
051700000613     O                                           +1 'Traduzione TIVIN00R -'
051800040301     O                                           +1 'FIVATWWR'
051900040301     O                                           +1 'Dettaglio barcode'
052000000616     O          E            testdett    3
052100000613     O                                           +2 'N° rec'
052200000613     O                                           +3 'Anteprima contenuto'
052300000616     O          E            rigadett    2
052400000613     O                       rrnum               +2
052500000621     O                       recko               +3
052600000616     O          E            findett     2
052700000613     O                       wrkdata
052800000613     O                       wrkora              +1
052900000613     O                       procname            +1
053000000616     O          E            findett     2
053100000613     O                                              'Cliente..................:'
053200040301     O                       VATCCM        z     +1
053300000616     O          E            findett     2
053400000613     O                                              'Riferimento Strategi.....:'
053500000613     O                       vlrhdl              +1
053600000616     O          E            findett     2
053700000613     O                                              'Giusti...................:'
053800000613     O                       §CTROK        2   +  1
053900000616     O          E            findett     2
054000000613     O                                              'Sbagliati e corretti.....:'
054100000613     O                       §CTRMO        2   +  1
054200000616     O          E            findett     2
054300000613     O                                              'Sbagliati e scartati.....:'
054400000613     O                       §CTRNO        2   +  1
054500000613** CMD - COMANDI CL
054600120116OVRPRTF OVRSCOPE(*CALLLVL) FILE(TITV2J6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
054700120116OVRPRTF OVRSCOPE(*CALLLVL) FILE(TITV2J6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
054800120116DLTOVR FILE(TITV2J6P TITV2J6PS) LVL(*)
054900010606
055000000613
