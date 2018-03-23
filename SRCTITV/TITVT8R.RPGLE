000100051219      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600040301     FFIVATwwr  O    E             DISK    usropn
000700060630     Ftitvt8p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900060630     Ftitvt8ps  O    f  198        PRINTER usropn
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
009500060630     C                   if        not %open(titvt8ps)
009600060630     C                   open      titvt8ps
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
011400060630     C                   if        %open(titvt8ps)
011500000616     C                   except    findett
011600060630     C                   close     titvt8ps
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
013500060630     C                   if        not %open(titvt8p)
013600060630     C                   open      titvt8p
013700990915     C                   endif
013800990915     C*
013900990915     C                   except    riepilogo
014000990915     C*
014100060630     C                   if        %open(titvt8p)
014200060630     C                   close     titvt8p
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
030000060630     C                   EVAL      VATCCM = 0053845
030100060630     C                   EVAL      VATLNP = 005
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
033300060630     C                   EVAL      VATNOT = %trim(%subst(vindta:117:35))
033400040301     C*
033500040301     C* Reperimento campi NUMERICI
033600060630     C                   EVAL      PiStr=%trim(%subst(vindta:47:7))
033700040301     C                   EXSR      CHKNUM
033800040301     C                   IF        PiInt=*on
033900040301     C                   Z-ADD     PiVal         VATNSP
034000040301     C                   ELSE
034100040301     C                   SETON                                        31
034200040301     C                   Z-ADD     *zeros        VATNSP
034300040301     C                   EVAL      vinmsg = %trimr(vinmsg)
034400040301     C                             + ' ' + 'VATNSP'
034500040301     C                   ENDIF
034600040301     C*
034700040301     C* Scarico il buffer del FIVAT
034800040301     C  N31              WRITE     FIVAT000
034900020204     C*
035000010607     C                   ENDSR
035100010607     C*----------------------------------------------------*
035200020204
035300010330
035400010330
035500010330     C*----------------------------------------------------*
035600010330     C*  CONTROLLO NUMERICITA' CAMPI
035700010330     C*----------------------------------------------------*
035800010330     C     CHKNUM        BEGSR
035900010330     C*
036000010606     C                   IF        PiDecChr = *blanks
036100010606     C                   EVAL      PiDecChr = ','
036200010606     C                   ENDIF
036300010606     C*
036400010606     C                   CALL(e)   'ISNUMERIC'
036500010330     C                   PARM                    PiStr            30
036600010606     C                   PARM                    PiDecChr          1
036700010330     C                   PARM      *ZEROS        PiVal            30 9
036800010330     C                   PARM      '0'           PiInt             1
036900010330     C                   PARM      '0'           PiNum             1
037000010330     C                   IF        %error
037100010606     C                   EVAL      PiNum=*off
037200010330     C                   ENDIF
037300010330     C*
037400010330     C                   ENDSR
037500010330     C***
037600010330
037700010601
037800010601
037900010601
038000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
038100010601     C     repfil        BEGSR
038200010601     C*
038300010601     C                   if        invfil = *zeros and
038400010601     C                             depfil > *zeros and
038500010629     C                             (vinflg = *blanks or
038600010629     C                              vinflg = *zeros)
038700010601     C
038800010601     C                   eval      invfil = depfil
038900010601     C                   endif
039000010601     C*
039100010601     C                   if        depfil <> invfil and
039200010601     C                             invfil > *zeros
039300010601     C                   eval      flgMulti = '1'
039400010601     C                   if        vinflg = *blanks
039500010601     C                   add       1             cntNonEl
039600010601     C                   endif
039700010601     C                   endif
039800010601     C*
039900010601     C                   if        vinflg = '2'
040000010601     C                   eval      flgStato = '2'
040100010601     C                   endif
040200010601     C*
040300010601     C                   ENDSR
040400010601     C***
040500010601
040600010601
040700010601
040800010330
040900010330
041000010330
041100990920      /TITLE Invio dei dati al punto operativo.
041200000613     C     invio         BEGSR
041300990920     C*
041400990920     C                   reset                   dscmz
041500010601     C                   move      invfil        cmzdst
041600990920     C                   eval      cmzfld = vlrfou
041700990920     C                   eval      cmzmbd = vlrhdl
041800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
041900000710     C                   if        prmfir = *blanks
042000040301     C                   eval      cmzfla = 'FIVAT00F'
042100040301     C                   eval      cmzmba = 'FIVAT00F'
042200000710     C                   else
042300000710     C                   eval      cmzfla = prmfir
042400000710     C                   eval      cmzmba = prmfir
042500000710     C                   endif
042600990920     C                   eval      cmznrr = *zeros
042700990920     C                   move      §ctrok        cmznrr
042800021018     C                   eval      cmzlba = vlrfl1
042900990920     C                   call(e)   'TIS711C'
043000990920     C                   parm                    dscmz
043100990921     C                   parm      *blanks       esito
043200990920     C                   if        %error
043300990920     C                             or cmzerr = '1'
043400990921     C                             or esito  = '1'
043500000710     C                   eval      wrkesito = '3'
043600990920     C                   endif
043700990920     C*
043800000613     C                   ENDSR
043900990910
044000010601
044100010601
044200010601
044300010601
044400010601      /TITLE Invio dei dati al punto operativo.
044500010601     C     opeini        BEGSR
044600010601     C*
044700010601     C* Inizializzo flag e contatori operativi
044800010601     C                   movel     '0'           flgGiro           1
044900010601     C                   movel     '0'           flgMulti          1
045000010601     C                   movel     '1'           flgStato          1
045100010615     C                   movel     '0'           flgOk             1
045200010601     C                   z-add     *zeros        cntNonEl         10 0
045300010601     C                   z-add     *zeros        depfil            3 0
045400010601     C                   z-add     *zeros        invfil            3 0
045500010601     C*
045600010601     C                   ENDSR
045700010601     C***
045800010601
045900010601
046000010601
046100010330
046200010330
046300000613     C     *inzsr        BEGSR
046400990910     C*
046500990910     C     *entry        plist
046600990920     C                   parm                    tivlrds
046700990921     C                   parm      wrkesito      esito
046800000724     C                   parm                    prmlit
046900000710     C                   parm                    prmfir
047000010330     C*
047100010330     C* CALCOLA LA DATA CORRENTE
047200010330     C                   time                    wn14             14 0
047300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
047400010330     C                   z-add     wn8           g08dat
047500010330     C                   z-add     *zeros        g08inv
047600010330     C                   movel     '0'           g08err
047700010330     C                   call      'XSRDA8'
047800010330     C                   parm                    wlbda8
047900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
048000000613     C*
048100000613     C                   ENDSR
048200000613     C***
048300990908
048400060630     Otitvt8p   E            riepilogo         2
048500990915     O                                              'Upload via Internet'
048600990915     O                                           +1 'Traduzione TIVIN00R -'
048700040301     O                                           +1 'FIVATWWR'
048800040301     O                                           +1 'Dettaglio barcode'
048900990915     O          E            riepilogo   2
049000990915     O                       wrkdata
049100990915     O                       wrkora              +1
049200990915     O                       procname            +1
049300990915     O          E            riepilogo   2
049400990915     O                                              'Cliente..................:'
049500040301     O                       VATCCM        z     +1
049600990915     O          E            riepilogo   2
049700990920     O                                              'Riferimento Strategi.....:'
049800990920     O                       vlrhdl              +1
049900990915     O          E            riepilogo   2
050000990915     O                                              'Giusti...................:'
050100971022     O                       §CTROK        2   +  1
050200990915     O          E            riepilogo   2
050300990915     O                                              'Sbagliati e corretti.....:'
050400971022     O                       §CTRMO        2   +  1
050500990915     O          E            riepilogo   2
050600990915     O                                              'Sbagliati e scartati.....:'
050700971022     O                       §CTRNO        2   +  1
050800000613
050900060630     Otitvt8ps  E            testdett          2
051000000613     O                                              'Upload via Internet'
051100000613     O                                           +1 'Traduzione TIVIN00R -'
051200040301     O                                           +1 'FIVATWWR'
051300040301     O                                           +1 'Dettaglio barcode'
051400000616     O          E            testdett    3
051500000613     O                                           +2 'N° rec'
051600000613     O                                           +3 'Anteprima contenuto'
051700000616     O          E            rigadett    2
051800000613     O                       rrnum               +2
051900000621     O                       recko               +3
052000000616     O          E            findett     2
052100000613     O                       wrkdata
052200000613     O                       wrkora              +1
052300000613     O                       procname            +1
052400000616     O          E            findett     2
052500000613     O                                              'Cliente..................:'
052600040301     O                       VATCCM        z     +1
052700000616     O          E            findett     2
052800000613     O                                              'Riferimento Strategi.....:'
052900000613     O                       vlrhdl              +1
053000000616     O          E            findett     2
053100000613     O                                              'Giusti...................:'
053200000613     O                       §CTROK        2   +  1
053300000616     O          E            findett     2
053400000613     O                                              'Sbagliati e corretti.....:'
053500000613     O                       §CTRMO        2   +  1
053600000616     O          E            findett     2
053700000613     O                                              'Sbagliati e scartati.....:'
053800000613     O                       §CTRNO        2   +  1
053900000613** CMD - COMANDI CL
054000060630OVRPRTF OVRSCOPE(*CALLLVL) FILE(TITVT8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
054100060630OVRPRTF OVRSCOPE(*CALLLVL) FILE(TITVT8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
054200060630DLTOVR FILE(TITVT8P TITVT8PS) LVL(*)
054300010606
054400000613
