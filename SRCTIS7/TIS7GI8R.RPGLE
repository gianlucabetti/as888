000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700010426     Ftis7gi8p  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900010426     Ftis7gi8ps O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100990908
001200000512     D*------------
001300000512     D* COMANDI
001400000512     D*------------
001500011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600000801     D*----------------------------------------------------
001700010215     D* DICHIARAZIOINE VARIABILI DI WRK
001800000801     D*----------------------------------------------------
001900990920     D dscmz         e ds                  inz
002000990910     D psds           sds
002100990910     D  procname         *PROC
002200990920     D tivlrds       e ds                  extname(tivlr00f)
002300990910     D esito           s              1
002400000724     D prmlit          s             10
002500000710     D prmfir          s             10
002600990921     D wrkesito        s                   like(esito)
002700990915     D wrkdata         s               d
002800990915     D wrkora          s               t
002900000613     D rrnum           s              6  0 INZ(*zeros)
003000000621     D recko           s            150    INZ(*blanks)
003100011123     D depcmd          s            150    INZ(*blanks)
003200010215     D depScat         s              5  0 INZ(*zeros)
003300010215     D depBaul         s              5  0 INZ(*zeros)
003400010215     D depPeso         s              7  2 INZ(*zeros)
003500010530     D depccm          s              7    INZ(*all'0')
003600000801
003700010213
003800000801     D  Num5_0         s              5  0
003900000830
004000000830     D*------------------
004100000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004200000830     D*------------------
004300000830     D WLBDA8          DS                  INZ
004400000830     D  G08DAT                 1      8  0
004500000830     D  G08INV                 9     16  0
004600000830     D  G08ERR                17     17
004700000830     D  G08TGI                18     22  0
004800000830     D*
004900990908
005000010201
005100010426     D*------------------
005200010426     D* DS DI RIDEFINIZIONE DEL CAMPO DATI FLAT RICEVUTO
005300010426     D*------------------
005400010426     D DSINPUT         DS                  INZ
005500010426     D  RAG                    1     35
005600010426     D  IND                   36     70
005700010427     D  CAP                   71     75
005800010426     D  LOC                   76    110
005900010426     D  PRV                  111    112
006000010427     D  COL                  113    113
006100010427     D  PES                  114    118
006200010426     D  TEL                  119    138
006300010426     D  NOTE                 139    173
006400010426     D  FRAASS               174    174
006500010427     D  CONTR                175    189
006600010426     D  ESPR                 190    190
006700010601     D  RIFALFA              191    205
006800010426     D*
006900010426
007000010426
007100010201
007200990915     C                   time                    wrkdata
007300990915     C                   time                    wrkora
007400000913     C                   reset                   rrnum
007500990921     C                   reset                   esito
007600990921     C                   reset                   wrkesito
007700000724     C*
007800000724     C* SE OCCORRE SPEDIRE IN FILIALE
007900000724     C                   if        vlrpoi <> *zeros
008000000724     C*
008100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008300000724     C     vlrpoi        chain     azorg01l
008400000724     C                   if        %found
008500000616     C                   movel(p)  CMD(1)        depcmd
008600020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008700000616     C*
008800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008900011123     C                   Z-ADD     150           LENGH            15 5
009000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009100000616     C                   PARM                    depcmd
009200000616     C                   PARM                    LENGH
009300000724     C*
009400000724     C                   endif
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
010600000616     C*
010700010426     C                   if        not %open(tis7gi8ps)
010800010426     C                   open      tis7gi8ps
010900000616     C                   except    testdett
011000000613     C                   endif
011100000613     C*
011200010215     C                   EXSR      RWVAB                                        LETT/SCR. VAB
011300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011400010202     C*
011500010426     C                   if        %open(tis7gi8ps)
011600000616     C                   except    findett
011700010426     C                   close     tis7gi8ps
011800000613     C                   endif
011900000616     C*
012000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012100011123     C                   Z-ADD     150           LENGH            15 5
012200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012300000616     C                   PARM                    CMD(3)
012400000616     C                   PARM                    LENGH
012500000616     C*
012600000801     C
012700010201     C                   seton                                        LR
012800000613
012900000613
013000000613     C*--------------------------------------------------------
013100000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013200000613     C*--------------------------------------------------------
013300000621     C     STPR          BEGSR
013400000613     C*
013500010426     C                   if        not %open(tis7gi8p)
013600010426     C                   open      tis7gi8p
013700990915     C                   endif
013800990915     C*
013900990915     C                   except    riepilogo
014000990915     C*
014100010426     C                   if        %open(tis7gi8p)
014200010426     C                   close     tis7gi8p
014300990914     C                   endif
014400990910     C*
014500000613     C                   ENDSR
014600000613     C***
014700990908
014800000801
014900910830     C*--------------------------------------------------------
015000021113     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWR              *
015100910830     C*--------------------------------------------------------
015200010215     C     RWVAB         BEGSR
015300990910     C*
015400990914     C                   if        not %open(tivin00r)
015500990908     C                   open      tivin00r
015600990914     C                   endif
015700021113     C                   if        not %open(fivabwwr)
015800021113     C                   open      fivabwwr
015900990914     C                   endif
016000990910     C*
016100010215     C                   clear                   §CTROK            5 0
016200000801     C                   clear                   §CTRMO            5 0
016300000801     C                   clear                   §CTRNO            5 0
016400990910     C*
016500921023     C                   DO        *HIVAL
016600990913     C*
016700990915     C                   READ      tivin00r                               70
016800000905     C                   if        vindta > *blanks
016900000613     C                   add       1             rrnum
017000000801     C*
017100000801     C                   if        *in70 = *off
017200000801     C                             and
017300000801     C                             (vinflg = *blanks
017400000801     C                              or vinflg = '0'
017500000801     C                              or vinflg = '2')
017600000801     C*
017700000801     C                   clear                   vinmsg
017800000801     C                   eval      vinflg = '1'
017900010201     C*
018000000801     C                   exsr      inzvar
018100000801     C                   exsr      defcam
018200010201     C                   exsr      impvab
018300021113     C                   write     fivab000
018400010201     C                   endif
018500000905     C*
018600000905     C                   else
018700000905     C                   eval      vinflg = '1'
018800000905     C                   endif
018900000905     C*
019000000905     C  N70              update    tivin000
019100000905     C*
019200991022     C  N70              ENDdo
019300010202     C*
019400990910
019500990910     C* Se non ci sono record con errori ...
019600000710     C                   if        §ctrno = 0
019700990910     C* ... restituisco esito OK.
019800990921     C                   eval      wrkesito = '0'
019900990910     C                   else
020000010215     C                   if        §ctrok   > 0
020100990921     C                   eval      wrkesito = '1'
020200000710     C                   else
020300000710     C                   eval      wrkesito = '2'
020400990910     C                   endif
020500000710     C                   endif
020600990910     C*
020700990914     C                   if        %open(tivin00r)
020800990908     C                   close     tivin00r
020900990914     C                   endif
021000021113     C                   if        %open(fivabwwr)
021100021113     C                   close     fivabwwr
021200990914     C                   endif
021300990910     C*
021400010215     C                   if        §ctrok > 0
021500000724     C                             and vlrpoi <> *zeros
021600010202     C                   exsr      invio
021700990920     C                   endif
021800990920     C*
021900910830     C                   ENDSR
022000000613     C***
022100990920
022200000801     C*----------------------------------------------------*
022300010215     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022400000801     C*----------------------------------------------------*
022500010201     C     INZVAR        BEGSR
022600000801     C*
022700000801     C*
022800000801     C                   ENDSR
022900010427
023000000801     C*----------------------------------------------------*
023100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
023200000801     C*----------------------------------------------------*
023300000801     C     DEFCAM        BEGSR
023400000801     C*
023500010530     C                   EVAL      depccm = %trim(vlrppt)
023600010530     C                   MOVE(P)   depccm        VABCCM
023700010530     C*                  Z-ADD     0400052       VABCCM
023800010426     C                   Z-ADD     040           VABLNP
023900010426     C                   Z-ADD     000           VABCTR
024000000801     C*
024100000801     C                   ENDSR
024200000801     C*----------------------------------------------------*
024300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024400000801     C*----------------------------------------------------*
024500010201     C     IMPVAB        BEGSR
024600000801     C*
024700000801     C                   Z-ADD     *zeros        errore            1 0
024800010426     C*
024900010426     C* Muovo il contenuto del campo dati del file di input nella relativa DS x la ridefinizione
025000010426     C                   MOVEL(p)  vindta        DSINPUT
025100010426     C*
025200010427     C* Normalizzo la DS
025300010427     C                   EXSR      NORMDS
025400010427     C*
025500000830     C                   MOVEL     datcor        VABAAS
025600000830     C                   MOVE      datcor        VABMGS
025601021113     C                   MOVE(P)   vlrpoi        VABFGS
025700010427     C*
025800010427     C* Valorizzazione campi ALFA
025900010427     C                   MOVEL     RAG           VABRSD
026000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026200020117     C* ==
026300010427     C                   MOVEL     IND           VABIND
026400010427     C                   MOVEL     LOC           VABLOD
026500010427     C                   MOVEL     PRV           VABPRD
026600010427     C                   MOVEL     ESPR          VABTSP
026700010427     C                   MOVEL     NOTE          VABNT2
026800010427     C                   MOVEL     TEL           VABNOT
026900010601     C                   MOVEL     RIFALFA       VABRMA
027000010427     C*
027100010427     C                   EVAL      PiStr=CAP
027200010427     C                   EXSR      CHKNUM
027300010502     C                   IF        PiInt=*on
027400010502     C                   Z-ADD     PiVal         Num5_0
027500010502     C                   MOVEL(p)  Num5_0        VABCAD
027600010427     C                   ELSE
027700010427     C                   ADD       1             errore
027800010427     C                   EVAL      vinmsg = %trimr(vinmsg)
027900010427     C                             + ' ' + 'VABCAD'
028000010427     C                   ENDIF
028100010427     C*
028200010427     C                   EVAL      PiStr=COL
028300010427     C                   EXSR      CHKNUM
028400010427     C                   IF        PiInt=*on
028500010427     C                   Z-ADD     PiVal         VABNCL
028600010427     C                   ELSE
028700010427     C                   ADD       1             errore
028800010427     C                   EVAL      vinmsg = %trimr(vinmsg)
028900010427     C                             + ' ' + 'VABNCL'
029000010427     C                   ENDIF
029100010427     C*
029200010427     C                   EVAL      PiStr=PES
029300010427     C                   EXSR      CHKNUM
029400010427     C                   IF        PiNum=*on
029500010427     C                   Z-ADD     PiVal         VABPKB
029600010427     C                   ELSE
029700010427     C                   ADD       1             errore
029800010427     C                   EVAL      vinmsg = %trimr(vinmsg)
029900010427     C                             + ' ' + 'VABPKB'
030000010427     C                   ENDIF
030100010427     C*
030200011115     C*                  EVAL      PiStr=CONTR
030300011115     C*                  EXSR      CHKNUM
030400011115     C*                  IF        PiNum=*on
030500011115     C*                  Z-ADD     PiVal         VABCAS
030600011115     C*                  ELSE
030700011115     C*                  ADD       1             errore
030800011115     C*                  EVAL      vinmsg = %trimr(vinmsg)
030900011115     C*                            + ' ' + 'VABCAS'
031000011115     C*                  ENDIF
031100010427     C*
031200010427     C* Altri campi
031300010427     C                   ADD       1             VABRMN
031400010426     C                   MOVEL     '1 '          VABCBO
031500010426     C     FRAASS        IFEQ      '2'
031600010426     C                   MOVEL     '2 '          VABCBO                         ASS.TO
031700010426     C                   ENDIF
031800010426     C     VABCAS        IFGT      *ZERO
031900010426     C     VABCBO        IFEQ      '1 '
032000010426     C                   MOVEL     '4 '          VABCBO                         FRANCO+C/A
032100010426     C                   ELSE
032200010426     C                   MOVEL     '6 '          VABCBO                         ASS.TO+C/A
032300010426     C                   ENDIF
032400010426     C                   ENDIF
032500010426     C*
032600000801     C* Ebbene...
032700000801     C                   ADD       1             §CTRMO
032800010201     C                   IF        errore <> *zeros
032900000801     C                   ADD       1             §CTRNO
033000000801     C                   EVAL      recko = vindta
033100000801     C                   EXCEPT    rigadett
033200000801     C                   EVAL      vinflg = '2'
033300000801     C                   ELSE
033400010215     C                   ADD       1             §CTROK
033500000801     C                   ENDIF
033600000801     C*
033700000801     C                   ENDSR
033800010427     C*----------------------------------------------------*
033900010427     C*  NORMALIZZAZIONE CAMPI A *BLANKS
034000010427     C*----------------------------------------------------*
034100010427     C     NORMDS        BEGSR
034200010427     C*
034300010427     C                   IF        CAP = *blanks
034400010427     C                   EVAL      CAP = *zeros
034500010427     C                   ENDIF
034600010427     C*
034700010427     C                   IF        COL = *blanks
034800010427     C                   EVAL      COL = *zeros
034900010427     C                   ENDIF
035000010427     C*
035100010427     C                   IF        PES = *blanks
035200010427     C                   EVAL      PES = *zeros
035300010427     C                   ENDIF
035400010427     C*
035500010427     C                   IF        CONTR = *blanks
035600010427     C                   EVAL      CONTR = *zeros
035700010427     C                   ENDIF
035800010427     C*
035900010427     C                   ENDSR
036000010427     C*----------------------------------------------------*
036100010427     C*  CONTROLLO NUMERICITA' CAMPI
036200010427     C*----------------------------------------------------*
036300010427     C     CHKNUM        BEGSR
036400010427     C*
036500010427     C                   call(e)   'ISNUMERIC'
036600010427     C                   PARM                    PiStr            30
036700010427     C                   PARM      ','           PiDecChr          1
036800010427     C                   PARM      *ZEROS        PiVal            30 9
036900010427     C                   PARM      '0'           PiInt             1
037000010427     C                   PARM      '0'           PiNum             1
037100010427     C                   IF        %error
037200010427     C                   EVAL      PiInt=*off
037300010427     C                   ENDIF
037400010427     C*
037500010427     C                   ENDSR
037600010427     C***
037700010427
037800010427
037900000801
038000000801
038100000801
038200990920      /TITLE Invio dei dati al punto operativo.
038300010202     C     invio         BEGSR
038400010201     C*
038500021113     C* Invio FIVAB
038600010201     C                   reset                   dscmz
038700010201     C                   move      vlrpoi        cmzdst
038800010201     C                   eval      cmzfld = vlrfou
038900010201     C                   eval      cmzmbd = vlrhdl
039000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
039100010201     C                   if        prmfir = *blanks
039200021113     C                   eval      cmzfla = 'FIVAB00F'
039300021113     C                   eval      cmzmba = 'FIVAB00F'
039400010201     C                   else
039500010201     C                   eval      cmzfla = prmfir
039600010201     C                   eval      cmzmba = prmfir
039700010201     C                   endif
039800010201     C                   eval      cmznrr = *zeros
039900010215     C                   move      §ctrok        cmznrr
040000021025     C                   eval      cmzlba = vlrfl1
040100010201     C                   call(e)   'TIS711C'
040200010201     C                   parm                    dscmz
040300010201     C                   parm      *blanks       esito
040400010201     C                   if        %error
040500010201     C                             or cmzerr = '1'
040600010201     C                             or esito  = '1'
040700010201     C                   eval      wrkesito = '3'
040800010205     C                   endif
040900990920     C*
041000000613     C                   ENDSR
041100000613     C***
041200990910
041300010426
041400010426
041500010426
041600000613     C     *inzsr        BEGSR
041700990910     C*
041800990910     C     *entry        plist
041900990920     C                   parm                    tivlrds
042000990921     C                   parm      wrkesito      esito
042100000724     C                   parm                    prmlit
042200000710     C                   parm                    prmfir
042300000613     C*
042400000830     C* CALCOLA LA DATA CORRENTE
042500000830     C                   time                    wn14             14 0
042600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
042700000830     C                   z-add     wn8           g08dat
042800000830     C                   z-add     *zeros        g08inv
042900000830     C                   movel     '0'           g08err
043000000830     C                   call      'XSRDA8'
043100000830     C                   parm                    wlbda8
043200000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
043300000830     C*
043400000613     C                   ENDSR
043500000613     C***
043600990908
043700010426     Otis7gi8p  E            riepilogo         2
043800990915     O                                              'Upload via Internet'
043900990915     O                                           +1 'Traduzione TIVIN00R -'
044000021113     O                                           +1 'FIVABWWR'
044100010201     O                                           +1 'Report testate bolle'
044200990915     O          E            riepilogo   2
044300990915     O                       wrkdata
044400990915     O                       wrkora              +1
044500990915     O                       procname            +1
044600990915     O          E            riepilogo   2
044700990915     O                                              'Cliente..................:'
044800990915     O                       VABCCM        z     +1
044900990915     O          E            riepilogo   2
045000990920     O                                              'Riferimento Strategi.....:'
045100990920     O                       vlrhdl              +1
045200990915     O          E            riepilogo   2
045300990915     O                                              'Giusti...................:'
045400010215     O                       §CTROK        2   +  1
045500990915     O          E            riepilogo   2
045600990915     O                                              'Sbagliati e corretti.....:'
045700971022     O                       §CTRMO        2   +  1
045800990915     O          E            riepilogo   2
045900990915     O                                              'Sbagliati e scartati.....:'
046000971022     O                       §CTRNO        2   +  1
046100000613
046200010426     Otis7gi8ps E            testdett          2
046300000613     O                                              'Upload via Internet'
046400000613     O                                           +1 'Traduzione TIVIN00R -'
046500021113     O                                           +1 'FIVABWWR'
046600010201     O                                           +1 'Report testate bolle'
046700000616     O          E            testdett    3
046800000613     O                                           +2 'N° rec'
046900000613     O                                           +3 'Anteprima contenuto'
047000000616     O          E            rigadett    2
047100000613     O                       rrnum               +2
047200000621     O                       recko               +3
047300000616     O          E            findett     2
047400000613     O                       wrkdata
047500000613     O                       wrkora              +1
047600000613     O                       procname            +1
047700000616     O          E            findett     2
047800000613     O                                              'Cliente..................:'
047900000613     O                       VABCCM        z     +1
048000000616     O          E            findett     2
048100000613     O                                              'Riferimento Strategi.....:'
048200000613     O                       vlrhdl              +1
048300000616     O          E            findett     2
048400000613     O                                              'Giusti...................:'
048500010215     O                       §CTROK        2   +  1
048600000616     O          E            findett     2
048700000613     O                                              'Sbagliati e corretti.....:'
048800000613     O                       §CTRMO        2   +  1
048900000616     O          E            findett     2
049000000613     O                                              'Sbagliati e scartati.....:'
049100000613     O                       §CTRNO        2   +  1
049200000512** CMD - COMANDI CL
049300011123OVRPRTF FILE(TIS7GI8P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
049400011123OVRPRTF FILE(TIS7GI8PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
049500010426DLTOVR FILE(TIS7GI8P TIS7GI8PS) LVL(*)
049600000512
049700000512
