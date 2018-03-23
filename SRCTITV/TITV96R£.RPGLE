000100060221      /TITLE Upload via Internet: traduzione in EDIVATWR. (x STEFANEL/NORDICA Cod. 1330192/0592101)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600060221     FEDIVATwr  O    E             DISK    usropn
000700040218     Ftitv96p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900040218     Ftitv96ps  O    f  198        PRINTER usropn
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
003200060221     D wNomeFile       s             30    INZ(*blanks)
003300060221     D wCMR            s             35    INZ(*blanks)
003400010330     D*------------------
003500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003600010330     D*------------------
003700010330     D WLBDA8          DS                  INZ
003800010330     D  G08DAT                 1      8  0
003900010330     D  G08INV                 9     16  0
004000010330     D  G08ERR                17     17
004100010330     D  G08TGI                18     22  0
004200000313     D*
004300010330
004400010330
004500010330
004600990915     C                   time                    wrkdata
004700990915     C                   time                    wrkora
004800000913     C                   reset                   rrnum
004900990921     C                   reset                   esito
005000990921     C                   reset                   wrkesito
005100010601     C*
005200010601     C                   exsr      opeini
005300020321     C                   exsr      rwvat
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
009700040218     C                   if        not %open(titv96ps)
009800040218     C                   open      titv96ps
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
010900010601
011000010601     C*--------------------------------------------------------
011100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011200010601     C*--------------------------------------------------------
011300010601     C     ENDELA        BEGSR
011400990915     C*
011500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011600000613     C*
011700040218     C                   if        %open(titv96ps)
011800000616     C                   except    findett
011900040218     C                   close     titv96ps
012000000613     C                   endif
012100000616     C*
012200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012300011113     C                   Z-ADD     150           LENGH            15 5
012400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012500000616     C                   PARM                    CMD(3)
012600000616     C                   PARM                    LENGH
012700000616     C*
012800010601     C                   ENDSR
012900010601     C***
013000010601
013100010601
013200010601
013300000613
013400000613     C*--------------------------------------------------------
013500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013600000613     C*--------------------------------------------------------
013700000621     C     STPR          BEGSR
013800000613     C*
013900040218     C                   if        not %open(titv96p)
014000040218     C                   open      titv96p
014100990915     C                   endif
014200990915     C*
014300990915     C                   except    riepilogo
014400990915     C*
014500040218     C                   if        %open(titv96p)
014600040218     C                   close     titv96p
014700990914     C                   endif
014800000613     C*
014900000613     C                   ENDSR
015000000613     C***
015100000613
015200010330
015300010601
015400010601
015500010330     C*--------------------------------------------------------
015600060221     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
015700010330     C*--------------------------------------------------------
015800020321     C     RWVAT         BEGSR
015900010330     C*
016000010330     C                   if        not %open(tivin00r)
016100010330     C                   open      tivin00r
016200010330     C                   endif
016300060221     C                   if        not %open(edivatwr)
016400060221     C                   open      edivatwr
016500010330     C                   endif
016600010330     C*
016700010604     C                   clear                   §CTROK
016800010604     C                   clear                   §CTRMO
016900010604     C                   clear                   §CTRNO
017000010330     C*
017100010330     C                   DO        *HIVAL
017200010330     C*
017300010330     C                   READ      tivin00r                               70
017400010618     C*
017500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017600010618     C                   if        vinflg = '1'
017700010618     C                   eval      flgOk = '1'
017800010618     C                   endif
017900010618     C*
018000010330     C                   if        vindta > *blanks
018100010330     C                   add       1             rrnum
018200010330     C*
018300010601     C                   if        *in70 = *off and
018400010330     C                             (vinflg = *blanks
018500010330     C                              or vinflg = '0'
018600010330     C                              or vinflg = '2')
018700010330     C*
018800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018900010711     C                   if        vinflg = *blanks or vinflg = '0'
019000010711     C                   clear                   vinmsg
019100010711     C                   endif
019200010601     C*
019300010330     C                   exsr      inzvar
019400010330     C                   exsr      defcam
019500020321     C                   exsr      impvat
019600010601     C*
019700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019800020321     C                   eval      depfil = VATLNP
019900010601     C                   exsr      repfil
020000010601     C                   if        depfil = invfil
020100021025     C                   if        vlrpoi = 999
020200021025     C                   MOVE(P)   invfil        VATFGS
020300021025     C                   else
020400021025     C                   MOVE(P)   vlrpoi        VATFGS
020500021025     C                   endif
020600010601     C*
020700010601     C                   exsr      PREELA
020800060221     C*
020900060221     C* VALORIZZO CAMPI RELATIVI AL "CMR"
021000060221     C                   EVAL      VATCMR = wCMR
021100060221     C                   EVAL      VATCNT = 1
021200010601     C*
021300010604     C* Ebbene...
021400010604     C*
021500010604     C  N31              ADD       1             §CTROK            7 0
021600010604     C   32              ADD       1             §CTRMO            7 0
021700010604     C   31              ADD       1             §CTRNO            7 0
021800020205     C*
021900020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
022000060221     C  N31              WRITE     EDIVAT00
022100010604     C*
022200010604     C                   if        *in31 = *off and
022300010604     C                             *in32 = *off
022400010604     C                   eval      vinflg = '1'
022500010604     C                   else
022600010604     C                   eval      recko = vindta
022700010604     C                   except    rigadett
022800010604     C                   eval      vinflg = '2'
022900010604     C                   endif
023000010604     C                   endif
023100010601     C*
023200010604     C                   endif
023300010604     C*
023400010330     C                   else
023500010330     C                   eval      vinflg = '1'
023600010330     C                   endif
023700010601     C*
023800010601     C  N70              update    tivin000
023900010330     C*
024000010330     C  N70              ENDdo
024100010601     C
024200010601     C*
024300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024400010601     C                   if        cntNonEl = *zeros or
024500010601     C                             flgMulti = '0'
024600010330     C* Se non ci sono record con errori ...
024700010601     C                   if        §ctrno = 0 and
024800010604     C                             §ctrmo = 0 and
024900010601     C                             flgStato <> '2'
025000010330     C* ... restituisco esito OK.
025100010330     C                   eval      wrkesito = '0'
025200010330     C                   else
025300010330     C                   if        §ctrok > 0
025400010330     C                   eval      wrkesito = '1'
025500010330     C                   else
025600010615     C                   if        flgOk = '0'
025700010615     C                   eval      wrkesito = '2'
025800010615     C                   else
025900010615     C                   eval      wrkesito = '6'
026000010615     C                   endif
026100010330     C                   endif
026200010330     C                   endif
026300010601     C                   else
026400010601     C                   eval      wrkesito = '9'
026500010601     C                   endif
026600010330     C*
026700010330     C                   if        %open(tivin00r)
026800010330     C                   close     tivin00r
026900010330     C                   endif
027000060221     C                   if        %open(edivatwr)
027100060221     C                   close     edivatwr
027200010330     C                   endif
027300010601     C*
027400010601     C                   if        vlrpoi <> 999
027500010601     C                   eval      invfil = vlrpoi
027600010601     C                   endif
027700010330     C*
027800010330     C                   if        §ctrok > 0
027900010601     C                             and invfil > *zeros
028000010330     C                   exsr      invio
028100010330     C                   endif
028200010601     C*
028300010618     C                   if        flgGiro = '1'
028400010601     C                   exsr      endela
028500010618     C                   endif
028600010330     C*
028700010330     C                   ENDSR
028800010330     C***
028900010330
029000010601
029100010601
029200010601
029300010330     C*----------------------------------------------------*
029400010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029500010330     C*----------------------------------------------------*
029600040218     C     INZVAR        BEGSR
029700010330     C*
029800010330     C*
029900010330     C                   ENDSR
030000010330     C*----------------------------------------------------*
030100010330     C*  IMPOSTAZIONE CAMPI COSTANTI
030200010330     C*----------------------------------------------------*
030300010330     C     DEFCAM        BEGSR
030400010330     C*
030500020204     C* Inizializzo il buffer del record da scrivere
030600060221     C                   CLEAR                   EDIVAT00
030700020204     C* Imposto i valori di default...
030800040218     C                   EVAL      VATCCM = 1330192
030900040218     C                   EVAL      VATLNP = 133
031000020321     C                   EVAL      VATTRC = 'E'
031100020204     C* ... e poi verifico se sono stati passati come parametri
031200020204     C                   IF        vlrppt > *blanks
031300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031400020204     C                   EXSR      CHKNUM
031500020204     C                   IF        PiInt=*on
031600020321     C                   Z-ADD     PiVal         VATCCM
031700020204     C                   ENDIF
031800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031900020204     C                   EXSR      CHKNUM
032000020204     C                   IF        PiInt=*on
032100020321     C                   Z-ADD     PiVal         VATLNP
032200020204     C                   ENDIF
032300020321     C                   ENDIF
032400020204     C*
032500010330     C                   ENDSR
032600010607     C*----------------------------------------------------*
032700060221     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
032800010607     C*----------------------------------------------------*
032900020321     C     IMPVAT        BEGSR
033000010607     C*
033100010607     C                   SETOFF                                       3132
033200040218     C*
033300040218     C                   MOVEL     datcor        VATAAS
033400010607     C*
033500010607     C* Reperimento campi ALFA
033600040218     C                   EVAL      VATNOT=%trim(%subst(vindta:42:18))
033700020304     C*
033800040218     C* VATNSP
033900040218     C                   EVAL      PiStr=%trim(%subst(vindta:121:5))
034000020304     C                   EXSR      CHKNUM
034100020304     C                   IF        PiInt=*on
034200020321     C                   Z-ADD     PiVal         VATNSP
034300020304     C                   ELSE
034400020304     C                   SETON                                        32
034500020321     C                   Z-ADD     *zeros        VATNSP
034600020304     C                   EVAL      vinmsg = %trimr(vinmsg)
034700020321     C                             + ' ' + 'VATNSP'
034800020304     C                   ENDIF
034900020204     C*
035000010607     C                   ENDSR
035100010607     C*----------------------------------------------------*
035200010601
035300010330
035400010330     C*----------------------------------------------------*
035500010330     C*  CONTROLLO NUMERICITA' CAMPI
035600010330     C*----------------------------------------------------*
035700010330     C     CHKNUM        BEGSR
035800010330     C*
035900010606     C                   IF        PiDecChr = *blanks
036000010606     C                   EVAL      PiDecChr = ','
036100010606     C                   ENDIF
036200010606     C*
036300010606     C                   CALL(e)   'ISNUMERIC'
036400010330     C                   PARM                    PiStr            30
036500010606     C                   PARM                    PiDecChr          1
036600010330     C                   PARM      *ZEROS        PiVal            30 9
036700010330     C                   PARM      '0'           PiInt             1
036800010330     C                   PARM      '0'           PiNum             1
036900010330     C                   IF        %error
037000010606     C                   EVAL      PiNum=*off
037100010330     C                   ENDIF
037200010330     C*
037300010330     C                   ENDSR
037400010330     C***
037500020403
037600020403
037700010601
037800010601
037900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
038000010601     C     repfil        BEGSR
038100010601     C*
038200010601     C                   if        invfil = *zeros and
038300010601     C                             depfil > *zeros and
038400010629     C                             (vinflg = *blanks or
038500010629     C                              vinflg = *zeros)
038600010601     C
038700010601     C                   eval      invfil = depfil
038800010601     C                   endif
038900010601     C*
039000010601     C                   if        depfil <> invfil and
039100010601     C                             invfil > *zeros
039200010601     C                   eval      flgMulti = '1'
039300010601     C                   if        vinflg = *blanks
039400010601     C                   add       1             cntNonEl
039500010601     C                   endif
039600010601     C                   endif
039700010601     C*
039800010601     C                   if        vinflg = '2'
039900010601     C                   eval      flgStato = '2'
040000010601     C                   endif
040100010601     C*
040200010601     C                   ENDSR
040300010601     C***
040400010601
040500010601
040600010601
040700010330
040800010330
040900010330
041000990920      /TITLE Invio dei dati al punto operativo.
041100000613     C     invio         BEGSR
041200990920     C*
041300990920     C                   reset                   dscmz
041400010601     C                   move      invfil        cmzdst
041500990920     C                   eval      cmzfld = vlrfou
041600990920     C                   eval      cmzmbd = vlrhdl
041700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
041800000710     C                   if        prmfir = *blanks
041900060221     C                   eval      cmzfla = 'EDIVAT0F'
042000060221     C                   eval      cmzmba = 'EDIVAT0F'
042100000710     C                   else
042200000710     C                   eval      cmzfla = prmfir
042300000710     C                   eval      cmzmba = prmfir
042400000710     C                   endif
042500990920     C                   eval      cmznrr = *zeros
042600990920     C                   move      §ctrok        cmznrr
042700021018     C                   eval      cmzlba = vlrfl1
042800990920     C                   call(e)   'TIS711C'
042900990920     C                   parm                    dscmz
043000990921     C                   parm      *blanks       esito
043100990920     C                   if        %error
043200990920     C                             or cmzerr = '1'
043300990921     C                             or esito  = '1'
043400000710     C                   eval      wrkesito = '3'
043500990920     C                   endif
043600990920     C*
043700000613     C                   ENDSR
043800990910
043900010601
044000010601
044100010601
044200010601
044300010601      /TITLE Invio dei dati al punto operativo.
044400010601     C     opeini        BEGSR
044500010601     C*
044600010601     C* Inizializzo flag e contatori operativi
044700010601     C                   movel     '0'           flgGiro           1
044800010601     C                   movel     '0'           flgMulti          1
044900010601     C                   movel     '1'           flgStato          1
045000010615     C                   movel     '0'           flgOk             1
045100010601     C                   z-add     *zeros        cntNonEl         10 0
045200010601     C                   z-add     *zeros        depfil            3 0
045300010601     C                   z-add     *zeros        invfil            3 0
045400010601     C*
045500010601     C                   ENDSR
045600010601     C***
045700010601
045800010601
045900010601
046000010330
046100010330
046200000613     C     *inzsr        BEGSR
046300990910     C*
046400990910     C     *entry        plist
046500990920     C                   parm                    tivlrds
046600990921     C                   parm      wrkesito      esito
046700000724     C                   parm                    prmlit
046800000710     C                   parm                    prmfir
046900010330     C*
047000010330     C* CALCOLA LA DATA CORRENTE
047100010330     C                   time                    wn14             14 0
047200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
047300010330     C                   z-add     wn8           g08dat
047400010330     C                   z-add     *zeros        g08inv
047500010330     C                   movel     '0'           g08err
047600010330     C                   call      'XSRDA8'
047700010330     C                   parm                    wlbda8
047800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
047900060221     C*
048000060221     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
048100060221     C                   move(p)   vlrMSG        wNomeFile
048200060221     C*
048300060221     C* Reperisco il CMR da attribuire alla transazione
048400060221     C                   eval      wCMR = %subst(wNomeFile:7:6)
048500000613     C*
048600000613     C                   ENDSR
048700000613     C***
048800990908
048900040218     Otitv96p   E            riepilogo         2
049000990915     O                                              'Upload via Internet'
049100990915     O                                           +1 'Traduzione TIVIN00R -'
049200060221     O                                           +1 'EDIVATWR'
049300990915     O                                           +1 'Testate bolle'
049400990915     O          E            riepilogo   2
049500990915     O                       wrkdata
049600990915     O                       wrkora              +1
049700990915     O                       procname            +1
049800990915     O          E            riepilogo   2
049900990915     O                                              'Cliente..................:'
050000020321     O                       VATCCM        z     +1
050100990915     O          E            riepilogo   2
050200990920     O                                              'Riferimento Strategi.....:'
050300990920     O                       vlrhdl              +1
050400990915     O          E            riepilogo   2
050500990915     O                                              'Giusti...................:'
050600971022     O                       §CTROK        2   +  1
050700990915     O          E            riepilogo   2
050800990915     O                                              'Sbagliati e corretti.....:'
050900971022     O                       §CTRMO        2   +  1
051000990915     O          E            riepilogo   2
051100990915     O                                              'Sbagliati e scartati.....:'
051200971022     O                       §CTRNO        2   +  1
051300000613
051400040218     Otitv96ps  E            testdett          2
051500000613     O                                              'Upload via Internet'
051600000613     O                                           +1 'Traduzione TIVIN00R -'
051700060221     O                                           +1 'EDIVATWR'
051800000613     O                                           +1 'Testate bolle'
051900000616     O          E            testdett    3
052000000613     O                                           +2 'N° rec'
052100000613     O                                           +3 'Anteprima contenuto'
052200000616     O          E            rigadett    2
052300000613     O                       rrnum               +2
052400000621     O                       recko               +3
052500000616     O          E            findett     2
052600000613     O                       wrkdata
052700000613     O                       wrkora              +1
052800000613     O                       procname            +1
052900000616     O          E            findett     2
053000000613     O                                              'Cliente..................:'
053100020321     O                       VATCCM        z     +1
053200000616     O          E            findett     2
053300000613     O                                              'Riferimento Strategi.....:'
053400000613     O                       vlrhdl              +1
053500000616     O          E            findett     2
053600000613     O                                              'Giusti...................:'
053700000613     O                       §CTROK        2   +  1
053800000616     O          E            findett     2
053900000613     O                                              'Sbagliati e corretti.....:'
054000000613     O                       §CTRMO        2   +  1
054100000616     O          E            findett     2
054200000613     O                                              'Sbagliati e scartati.....:'
054300000613     O                       §CTRNO        2   +  1
054400000613** CMD - COMANDI CL
054500040218OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV96P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
054600040218OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV96PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
054700040218DLTOVR FILE(TITV96P TITV96PS) LVL(*)
054800010606
054900000613
