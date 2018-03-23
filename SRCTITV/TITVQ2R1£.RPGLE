000100060112      /TITLE Upload via Internet: traduzione in EDIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600060112     FEDIVATwr  O    E             DISK    usropn
000700010705     Ftis7t9p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900010705     Ftis7t9ps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
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
003100011123     D depcmd          s            150    INZ(*blanks)
003200010330     D*------------------
003300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003400010330     D*------------------
003500010330     D WLBDA8          DS                  INZ
003600010330     D  G08DAT                 1      8  0
003700010330     D  G08INV                 9     16  0
003800010330     D  G08ERR                17     17
003900010330     D  G08TGI                18     22  0
004000010612     D*
004100010612     D fnvataar        ds                  inz
004200060109     D  a_vatatb                      1
004300060109     D  a_vatccm                      7
004400060109     D  a_vatlnp                      3
004500060109     D  a_vataas                      4
004600060109     D  a_vatmgs                      4
004700060109     D  a_vatnrs                      2
004800060109     D  a_vatnsp                      7
004900070326     D  a_vatldv              18     28
005000060109     D  a_vattrc                      1
005100071122     D* a_vatnot                     35
005200071122     D  a_vatnot                     31
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
006300010612     C                   exsr      rwvat
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
008700020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008800000724     C*
008900000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009000011123     C                   Z-ADD     150           LENGH            15 5
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
010200011123     C                   Z-ADD     150           LENGH            15 5
010300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010400000616     C                   PARM                    depcmd
010500000616     C                   PARM                    LENGH
010600000613     C*
010700010705     C                   if        not %open(tis7t9ps)
010800010705     C                   open      tis7t9ps
010900030407     C   50              except    testdett
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
012100010601     C*--------------------------------------------------------
012200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012300010601     C*--------------------------------------------------------
012400010601     C     ENDELA        BEGSR
012500990915     C*
012600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012700000613     C*
012800010705     C                   if        %open(tis7t9ps)
012900030407     C   50              except    findett
013000010705     C                   close     tis7t9ps
013100000613     C                   endif
013200000616     C*
013300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013400011123     C                   Z-ADD     150           LENGH            15 5
013500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013600000616     C                   PARM                    CMD(3)
013700000616     C                   PARM                    LENGH
013800000616     C*
013900010601     C                   ENDSR
014000010601     C***
014100010601
014200010601
014300010601
014400010601
014500000613
014600000613     C*--------------------------------------------------------
014700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014800000613     C*--------------------------------------------------------
014900000621     C     STPR          BEGSR
015000000613     C*
015100010705     C                   if        not %open(tis7t9p)
015200010705     C                   open      tis7t9p
015300990915     C                   endif
015400990915     C*
015500030407     C   50              except    riepilogo
015600990915     C*
015700010705     C                   if        %open(tis7t9p)
015800010705     C                   close     tis7t9p
015900990914     C                   endif
016000000613     C*
016100000613     C                   ENDSR
016200000613     C***
016300000613
016400010330
016500010601
016600010601
016700010601
016800010330     C*--------------------------------------------------------
016900060112     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
017000010330     C*--------------------------------------------------------
017100010612     C     RWVAT         BEGSR
017200010330     C*
017300010330     C                   if        not %open(tivin00r)
017400010330     C                   open      tivin00r
017500010330     C                   endif
017600060112     C                   if        not %open(edivatwr)
017700060112     C                   open      edivatwr
017800010330     C                   endif
017900010330     C*
018000010604     C                   clear                   §CTROK
018100010604     C                   clear                   §CTRMO
018200010604     C                   clear                   §CTRNO
018300010330     C*
018400010330     C                   DO        *HIVAL
018500010330     C*
018600010330     C                   READ      tivin00r                               70
018700010618     C*
018800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018900010618     C                   if        vinflg = '1'
019000010618     C                   eval      flgOk = '1'
019100010618     C                   endif
019200010618     C*
019300010330     C                   if        vindta > *blanks
019400010330     C                   add       1             rrnum
019500010330     C*
019600010601     C                   if        *in70 = *off and
019700010330     C                             (vinflg = *blanks
019800010330     C                              or vinflg = '0'
019900010330     C                              or vinflg = '2')
020000010330     C*
020100010330     C                   clear                   vinmsg
020200010613     C                   eval      fnvataar = vindta
020300010601     C*
020400010330     C                   exsr      inzvar
020500010330     C                   exsr      defcam
020600010612     C                   exsr      impvat
020700010601     C*
020800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020900010612     C                   eval      depfil = VATLNP
021000010601     C                   exsr      repfil
021100010601     C                   if        depfil = invfil
021200021014     C                   if        vlrpoi = 999
021300021010     C                   MOVE(P)   invfil        VATFGS
021400021014     C                   else
021500021014     C                   MOVE(P)   vlrpoi        VATFGS
021600021014     C                   endif
021700010601     C*
021800010601     C                   exsr      PREELA
021900060112     C*
022000060112     C* VALORIZZO CAMPI RELATIVI AL "CMR"
022100070417     C                   EVAL      VATCMR = %trim(a_vatldv)
022200060112     C                   EVAL      VATCNT = 1
022300010601     C*
022400010604     C* Ebbene...
022500010604     C*
022600010604     C  N31              ADD       1             §CTROK            7 0
022700010604     C   32              ADD       1             §CTRMO            7 0
022800010604     C   31              ADD       1             §CTRNO            7 0
022900060112     C  N31              WRITE     EDIVAT00
023000010604     C*
023100010604     C                   if        *in31 = *off and
023200010604     C                             *in32 = *off
023300010604     C                   eval      vinflg = '1'
023400010604     C                   else
023500010604     C                   eval      recko = vindta
023600010705     C                   if        %open(tis7t9ps)
023700030407     C   50              except    rigadett
023800010612     C                   endif
023900010604     C                   eval      vinflg = '2'
024000010604     C                   endif
024100010604     C                   endif
024200010601     C*
024300010604     C                   endif
024400010604     C*
024500010330     C                   else
024600010330     C                   eval      vinflg = '1'
024700010330     C                   endif
024800010601     C*
024900010601     C  N70              update    tivin000
025000010330     C*
025100010330     C  N70              ENDdo
025200010601     C
025300010601     C*
025400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025500010601     C                   if        cntNonEl = *zeros or
025600010601     C                             flgMulti = '0'
025700010330     C* Se non ci sono record con errori ...
025800010601     C                   if        §ctrno = 0 and
025900010604     C                             §ctrmo = 0 and
026000010601     C                             flgStato <> '2'
026100010330     C* ... restituisco esito OK.
026200010330     C                   eval      wrkesito = '0'
026300010330     C                   else
026400010330     C                   if        §ctrok > 0
026500010330     C                   eval      wrkesito = '1'
026600010330     C                   else
026700010615     C                   if        flgOk = '0'
026800010615     C                   eval      wrkesito = '2'
026900010615     C                   else
027000010615     C                   eval      wrkesito = '6'
027100010615     C                   endif
027200010330     C                   endif
027300010330     C                   endif
027400010601     C                   else
027500010601     C                   eval      wrkesito = '9'
027600010601     C                   endif
027700010330     C*
027800010330     C                   if        %open(tivin00r)
027900010330     C                   close     tivin00r
028000010330     C                   endif
028100060112     C                   if        %open(edivatwr)
028200060112     C                   close     edivatwr
028300010330     C                   endif
028400010601     C*
028500010601     C                   if        vlrpoi <> 999
028600010601     C                   eval      invfil = vlrpoi
028700010601     C                   endif
028800010330     C*
028900010330     C                   if        §ctrok > 0
029000010601     C                             and invfil > *zeros
029100010330     C                   exsr      invio
029200010330     C                   endif
029300010601     C*
029400010612     C                   if        flgGiro = '1'
029500010601     C                   exsr      endela
029600010612     C                   endif
029700010330     C*
029800010330     C                   ENDSR
029900010330     C***
030000010330
030100010601
030200010601
030300010601
030400010601
030500010330     C*----------------------------------------------------*
030600010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030700010330     C*----------------------------------------------------*
030800010330     C     INZVAR        BEGSR
030900010330     C*
031000010330     C*
031100010330     C                   ENDSR
031200010330     C*----------------------------------------------------*
031300010330     C*  IMPOSTAZIONE CAMPI COSTANTI
031400010330     C*----------------------------------------------------*
031500010330     C     DEFCAM        BEGSR
031600010330     C*
031700060112     C                   CLEAR                   EDIVAT00
031800010330     C*
031900010330     C                   ENDSR
032000010705     C*----------------------------------------------------*
032100010705
032200010705
032300010705
032400010705
032500010705     C*----------------------------------------------------*
032600060112     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
032700010705     C*----------------------------------------------------*
032800010705     C     IMPVAT        BEGSR
032900010705     C*
033000010705     C                   SETOFF                                       3132
033100010705     C*
033200010705     C* Reperimento campi ALFA
033300010705     C                   EVAL      VATATB=a_vatatb
033400010705     C                   EVAL      VATTRC=a_vattrc
033500020725     C                   EVAL      VATNOT=%trim(a_vatnot)
033600010705     C*
033700010705     C                   EVAL      PiStr=%trim(a_vatccm)
033800010705     C                   EXSR      CHKNUM
033900010705     C                   IF        PiInt=*on
034000010705     C                   Z-ADD     PiVal         VATCCM
034100010705     C                   ELSE
034200010705     C                   SETON                                        31
034300010705     C                   EVAL      vinmsg = %trimr(vinmsg)
034400010705     C                             + ' ' + 'VATCCM'
034500010705     C                   ENDIF
034600010705     C*
034700010705     C                   EVAL      PiStr=%trim(a_vatlnp)
034800010705     C                   EXSR      CHKNUM
034900010705     C                   IF        PiInt=*on
035000010705     C                   Z-ADD     PiVal         VATLNP
035100010705     C                   ELSE
035200010705     C                   SETON                                        31
035300010705     C                   EVAL      vinmsg = %trimr(vinmsg)
035400021010     C                             + ' ' + 'VATLNP'
035500010705     C                   ENDIF
035600010705     C*
035700060109     C                   EVAL      PiStr=%trim(a_vataas)
035800060109     C                   EXSR      CHKNUM
035900060109     C                   IF        PiInt=*on
036000060109     C                   Z-ADD     PiVal         VATAAS
036100060109     C                   ELSE
036200060109     C                   SETON                                        32
036300060109     C                   MOVEL     datcor        VATAAS
036400060109     C                   EVAL      vinmsg = %trimr(vinmsg)
036500060109     C                             + ' ' + 'VATAAS'
036600060109     C                   ENDIF
036700010705     C*
036800070215     C*                  EVAL      PiStr=%trim(a_vatnrs)
036900070215     C*                  IF        PiStr = *blanks
037000070215     C*                  EVAL      PiStr = *zeros
037100070215     C*                  ENDIF
037200070215     C*                  EXSR      CHKNUM
037300070215     C*                  IF        PiInt=*on
037400070215     C*                  Z-ADD     PiVal         VATNRS
037500070215     C*                  ELSE
037600070215     C*                  SETON                                        31
037700070215     C*                  EVAL      vinmsg = %trimr(vinmsg)
037800070215     C*                            + ' ' + 'VATNRS'
037900070215     C*                  ENDIF
038000010705     C*
038100010705     C                   EVAL      PiStr=%trim(a_vatnsp)
038200010705     C                   EXSR      CHKNUM
038300010705     C                   IF        PiInt=*on
038400010705     C                   Z-ADD     PiVal         VATNSP
038500010705     C                   ELSE
038600010705     C                   SETON                                        31
038700010705     C                   EVAL      vinmsg = %trimr(vinmsg)
038800010705     C                             + ' ' + 'VATNSP'
038900010705     C                   ENDIF
039000010705     C*
039100010705     C                   ENDSR
039200010705     C*----------------------------------------------------*
039300010705
039400010705
039500010705
039600010705
039700010705
039800010705     C*----------------------------------------------------*
039900010705     C*  CONTROLLO NUMERICITA' CAMPI
040000010705     C*----------------------------------------------------*
040100010705     C     CHKNUM        BEGSR
040200010705     C*
040300010705     C                   call(e)   'ISNUMERIC'
040400010705     C                   PARM                    PiStr            30
040500010705     C                   PARM      ','           PiDecChr          1
040600010705     C                   PARM      *ZEROS        PiVal            30 9
040700010705     C                   PARM      '0'           PiInt             1
040800010705     C                   PARM      '0'           PiNum             1
040900010705     C                   IF        %error
041000010705     C                   EVAL      PiInt=*off
041100010705     C                   ENDIF
041200010705     C*
041300010705     C                   ENDSR
041400010705     C*----------------------------------------------------*
041500010705
041600010705
041700010705
041800010601
041900010601
042000010601
042100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
042200010601     C     repfil        BEGSR
042300010601     C*
042400010601     C                   if        invfil = *zeros and
042500010601     C                             depfil > *zeros and
042600010613     C                             (vinflg = *blanks or
042700010613     C                              vinflg = *zeros)
042800010601     C
042900010601     C                   eval      invfil = depfil
043000010601     C                   endif
043100010601     C*
043200010601     C                   if        depfil <> invfil and
043300010601     C                             invfil > *zeros
043400010601     C                   eval      flgMulti = '1'
043500010601     C                   if        vinflg = *blanks
043600010601     C                   add       1             cntNonEl
043700010601     C                   endif
043800010601     C                   endif
043900010601     C*
044000010601     C                   if        vinflg = '2'
044100010601     C                   eval      flgStato = '2'
044200010601     C                   endif
044300010601     C*
044400010601     C                   ENDSR
044500010601     C***
044600010601
044700010601
044800010601
044900010330
045000010330
045100010330
045200990920      /TITLE Invio dei dati al punto operativo.
045300000613     C     invio         BEGSR
045400990920     C*
045500990920     C                   reset                   dscmz
045600010601     C                   move      invfil        cmzdst
045700990920     C                   eval      cmzfld = vlrfou
045800990920     C                   eval      cmzmbd = vlrhdl
045900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
046000000710     C                   if        prmfir = *blanks
046100060112     C                   eval      cmzfla = 'EDIVAT0F'
046200060112     C                   eval      cmzmba = 'EDIVAT0F'
046300000710     C                   else
046400000710     C                   eval      cmzfla = prmfir
046500000710     C                   eval      cmzmba = prmfir
046600000710     C                   endif
046700990920     C                   eval      cmznrr = *zeros
046800990920     C                   move      §ctrok        cmznrr
046900021003     C                   eval      cmzlba = vlrfl1
047000990920     C                   call(e)   'TIS711C'
047100990920     C                   parm                    dscmz
047200990921     C                   parm      *blanks       esito
047300990920     C                   if        %error
047400990920     C                             or cmzerr = '1'
047500990921     C                             or esito  = '1'
047600000710     C                   eval      wrkesito = '3'
047700990920     C                   endif
047800990920     C*
047900000613     C                   ENDSR
048000990910
048100010601
048200010601
048300010601
048400010601
048500010601      /TITLE Invio dei dati al punto operativo.
048600010601     C     opeini        BEGSR
048700010601     C*
048800010601     C* Inizializzo flag e contatori operativi
048900010601     C                   movel     '0'           flgGiro           1
049000010601     C                   movel     '0'           flgMulti          1
049100010601     C                   movel     '1'           flgStato          1
049200010615     C                   movel     '0'           flgOk             1
049300010601     C                   z-add     *zeros        cntNonEl         10 0
049400010601     C                   z-add     *zeros        depfil            3 0
049500010601     C                   z-add     *zeros        invfil            3 0
049600030407     C*
049700030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
049800030407     C                   if        %subst(vlrppt:1:1) = 'S'
049900030407     C                   seton                                        50
050000030407     C                   else
050100030407     C                   setoff                                       50
050200030407     C                   endif
050300010601     C*
050400010601     C                   ENDSR
050500010601     C***
050600010601
050700010601
050800010601
050900010330
051000010330
051100000613     C     *inzsr        BEGSR
051200990910     C*
051300990910     C     *entry        plist
051400990920     C                   parm                    tivlrds
051500990921     C                   parm      wrkesito      esito
051600000724     C                   parm                    prmlit
051700000710     C                   parm                    prmfir
051800010330     C*
051900010330     C* CALCOLA LA DATA CORRENTE
052000010330     C                   time                    wn14             14 0
052100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
052200010330     C                   z-add     wn8           g08dat
052300010330     C                   z-add     *zeros        g08inv
052400010330     C                   movel     '0'           g08err
052500010330     C                   call      'XSRDA8'
052600010330     C                   parm                    wlbda8
052700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
052800000613     C*
052900000613     C                   ENDSR
053000000613     C***
053100990908
053200010705     Otis7t9p   E            riepilogo         2
053300990915     O                                              'Upload via Internet'
053400990915     O                                           +1 'Traduzione TIVIN00R -'
053500060112     O                                           +1 'EDIVATWR'
053600990915     O                                           +1 'Testate bolle'
053700990915     O          E            riepilogo   2
053800990915     O                       wrkdata
053900990915     O                       wrkora              +1
054000990915     O                       procname            +1
054100990915     O          E            riepilogo   2
054200990915     O                                              'Cliente..................:'
054300010612     O                       VATCCM        z     +1
054400990915     O          E            riepilogo   2
054500990920     O                                              'Riferimento Strategi.....:'
054600990920     O                       vlrhdl              +1
054700990915     O          E            riepilogo   2
054800990915     O                                              'Giusti...................:'
054900971022     O                       §CTROK        2   +  1
055000990915     O          E            riepilogo   2
055100990915     O                                              'Sbagliati e corretti.....:'
055200971022     O                       §CTRMO        2   +  1
055300990915     O          E            riepilogo   2
055400990915     O                                              'Sbagliati e scartati.....:'
055500971022     O                       §CTRNO        2   +  1
055600000613
055700010705     Otis7t9ps  E            testdett          2
055800000613     O                                              'Upload via Internet'
055900000613     O                                           +1 'Traduzione TIVIN00R -'
056000060112     O                                           +1 'EDIVATWR'
056100000613     O                                           +1 'Testate bolle'
056200000616     O          E            testdett    3
056300000613     O                                           +2 'N° rec'
056400000613     O                                           +3 'Anteprima contenuto'
056500000616     O          E            rigadett    2
056600000613     O                       rrnum               +2
056700000621     O                       recko               +3
056800000616     O          E            findett     2
056900000613     O                       wrkdata
057000000613     O                       wrkora              +1
057100000613     O                       procname            +1
057200000616     O          E            findett     2
057300000613     O                                              'Cliente..................:'
057400010612     O                       VATCCM        z     +1
057500000616     O          E            findett     2
057600000613     O                                              'Riferimento Strategi.....:'
057700000613     O                       vlrhdl              +1
057800000616     O          E            findett     2
057900000613     O                                              'Giusti...................:'
058000000613     O                       §CTROK        2   +  1
058100000616     O          E            findett     2
058200000613     O                                              'Sbagliati e corretti.....:'
058300000613     O                       §CTRMO        2   +  1
058400000616     O          E            findett     2
058500000613     O                                              'Sbagliati e scartati.....:'
058600000613     O                       §CTRNO        2   +  1
058700000613** CMD - COMANDI CL
058800011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T9P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
058900011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T9PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
059000010705DLTOVR FILE(TIS7T9P TIS7T9PS) LVL(*)
059100010606
059200000613
