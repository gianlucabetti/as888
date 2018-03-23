000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700010219     Ftis7nott  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900010219     Ftis7notts O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100990908
001200000512     D*------------
001300000512     D* COMANDI
001400000512     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
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
003100011113     D depcmd          s            150    INZ(*blanks)
003200000801
003300010213
003400000801     D  Num5_0         s              5  0
003500000830
003600000830     D*------------------
003700000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003800000830     D*------------------
003900000830     D WLBDA8          DS                  INZ
004000000830     D  G08DAT                 1      8  0
004100000830     D  G08INV                 9     16  0
004200000830     D  G08ERR                17     17
004300000830     D  G08TGI                18     22  0
004400000830     D*
004500990908
004600010201
004700010201
004800990915     C                   time                    wrkdata
004900990915     C                   time                    wrkora
005000000913     C                   reset                   rrnum
005100990921     C                   reset                   esito
005200990921     C                   reset                   wrkesito
005300000724     C*
005400000724     C* SE OCCORRE SPEDIRE IN FILIALE
005500000724     C                   if        vlrpoi <> *zeros
005600000724     C*
005700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
005800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
005900000724     C     vlrpoi        chain     azorg01l
006000000724     C                   if        %found
006100000616     C                   movel(p)  CMD(1)        depcmd
006200020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
006300000616     C*
006400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
006500011113     C                   Z-ADD     150           LENGH            15 5
006600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
006700000616     C                   PARM                    depcmd
006800000616     C                   PARM                    LENGH
006900000724     C*
007000000724     C                   endif
007100000724     C                   endif
007200000616     C*
007300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007400000616     C                   movel(p)  CMD(2)        depcmd
007500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
007600000616     C*
007700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
007800011113     C                   Z-ADD     150           LENGH            15 5
007900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008000000616     C                   PARM                    depcmd
008100000616     C                   PARM                    LENGH
008200000616     C*
008300010219     C                   if        not %open(tis7notts)
008400010219     C                   open      tis7notts
008500000616     C                   except    testdett
008600000613     C                   endif
008700000613     C*
008800010215     C                   EXSR      RWVAB                                        LETT/SCR. VAB
008900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009000010202     C*
009100010219     C                   if        %open(tis7notts)
009200000616     C                   except    findett
009300010219     C                   close     tis7notts
009400000613     C                   endif
009500000616     C*
009600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
009700011113     C                   Z-ADD     150           LENGH            15 5
009800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009900000616     C                   PARM                    CMD(3)
010000000616     C                   PARM                    LENGH
010100000616     C*
010200000801     C
010300010201     C                   seton                                        LR
010400000613
010500000613
010600000613     C*--------------------------------------------------------
010700000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
010800000613     C*--------------------------------------------------------
010900000621     C     STPR          BEGSR
011000000613     C*
011100010219     C                   if        not %open(tis7nott)
011200010219     C                   open      tis7nott
011300990915     C                   endif
011400990915     C*
011500990915     C                   except    riepilogo
011600990915     C*
011700010219     C                   if        %open(tis7nott)
011800010219     C                   close     tis7nott
011900990914     C                   endif
012000990910     C*
012100000613     C                   ENDSR
012200000613     C***
012300990908
012400000801
012500910830     C*--------------------------------------------------------
012600021113     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWR              *
012700910830     C*--------------------------------------------------------
012800010215     C     RWVAB         BEGSR
012900990910     C*
013000990914     C                   if        not %open(tivin00r)
013100990908     C                   open      tivin00r
013200990914     C                   endif
013300021113     C                   if        not %open(fivabwwr)
013400021113     C                   open      fivabwwr
013500990914     C                   endif
013600990910     C*
013700010215     C                   clear                   §CTROK            5 0
013800000801     C                   clear                   §CTRMO            5 0
013900000801     C                   clear                   §CTRNO            5 0
014000990910     C*
014100921023     C                   DO        *HIVAL
014200990913     C*
014300990915     C                   READ      tivin00r                               70
014400000905     C                   if        vindta > *blanks
014500000613     C                   add       1             rrnum
014600000801     C*
014700000801     C                   if        *in70 = *off
014800000801     C                             and
014900000801     C                             (vinflg = *blanks
015000000801     C                              or vinflg = '0'
015100000801     C                              or vinflg = '2')
015200000801     C*
015300000801     C                   clear                   vinmsg
015400000801     C                   eval      vinflg = '1'
015500010201     C*
015600000801     C                   exsr      inzvar
015700000801     C                   exsr      defcam
015800010201     C                   exsr      impvab
015900021113     C                   write     fivab000
016000010201     C                   endif
016100000905     C*
016200000905     C                   else
016300000905     C                   eval      vinflg = '1'
016400000905     C                   endif
016500000905     C*
016600000905     C  N70              update    tivin000
016700000905     C*
016800991022     C  N70              ENDdo
016900010202     C*
017000990910
017100990910     C* Se non ci sono record con errori ...
017200000710     C                   if        §ctrno = 0
017300990910     C* ... restituisco esito OK.
017400990921     C                   eval      wrkesito = '0'
017500990910     C                   else
017600010215     C                   if        §ctrok   > 0
017700990921     C                   eval      wrkesito = '1'
017800000710     C                   else
017900000710     C                   eval      wrkesito = '2'
018000990910     C                   endif
018100000710     C                   endif
018200990910     C*
018300990914     C                   if        %open(tivin00r)
018400990908     C                   close     tivin00r
018500990914     C                   endif
018600021113     C                   if        %open(fivabwwr)
018700021113     C                   close     fivabwwr
018800990914     C                   endif
018900990910     C*
019000010215     C                   if        §ctrok > 0
019100000724     C                             and vlrpoi <> *zeros
019200010202     C                   exsr      invio
019300990920     C                   endif
019400990920     C*
019500910830     C                   ENDSR
019600000613     C***
019700990920
019800000801     C*----------------------------------------------------*
019900010215     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020000000801     C*----------------------------------------------------*
020100010219     C     INZVAR        BEGSR
020200000801     C*
020300010219     C                   Z-ADD     *zeros        Num5_0
020400000801     C*
020500000801     C                   ENDSR
020600000801     C*----------------------------------------------------*
020700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
020800000801     C*----------------------------------------------------*
020900000801     C     DEFCAM        BEGSR
021000000801     C*
021100021113     C                   CLEAR                   FIVAB000
021200010219     C                   Z-ADD     000           VABCTR
021300010219     C                   Z-ADD     0591254       VABCCM
021400010219     C                   Z-ADD     059           VABLNP
021500000801     C*
021600000801     C                   ENDSR
021700000801     C*----------------------------------------------------*
021800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
021900000801     C*----------------------------------------------------*
022000010201     C     IMPVAB        BEGSR
022100000801     C*
022200000801     C                   Z-ADD     *zeros        errore            1 0
022300000830     C                   MOVEL     datcor        VABAAS
022400000830     C                   MOVE      datcor        VABMGS
022401021113     C                   MOVE(P)   vlrpoi        VABFGS
022500010215     C*
022600010215     C* Reperimento campi ALFA
022700010219     C                   EVAL      VABRSD=%trim(%subst(vindta:202:35))
022800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
022900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
023000020117     C* ==
023100010219     C                   EVAL      VABRD2=%trim(%subst(vindta:237:35))
023200010219     C                   EVAL      VABIND=%trim(%subst(vindta:272:35))
023300010219     C                   EVAL      VABLOD=%trim(%subst(vindta:347:25))
023400010219     C                   EVAL      VABPRD=%trim(%subst(vindta:372:2))
023500010219     C                   EVAL      VABNOT=%trim(%subst(vindta:898:60))
023600010219     C*
023700010219     C                   EVAL      PiStr=%trim(%subst(vindta:342:5))
023800010219     C                   EXSR      CHKNUM
023900010219     C                   IF        PiInt=*on
024000010219     C                   Z-ADD     PiVal         Num5_0
024100010219     C                   MOVEL(p)  Num5_0        VABCAD
024200010219     C                   ELSE
024300010219     C                   ADD       1             errore
024400010219     C                   EVAL      vinmsg = %trimr(vinmsg)
024500010219     C                             + ' ' + 'VABCAD'
024600010219     C                   ENDIF
024700010219     C* Determino il codice cliente da attribuire
024800010219     C                   IF        %trim(%subst(vindta:377:3)) = 'POL'
024900010219     C                   Z-ADD     0590759       VABCCM
025000010219     C                   ENDIF
025100010219     C                   IF        %trim(%subst(vindta:377:3)) = 'BPB'
025200010219     C                   Z-ADD     0591254       VABCCM
025300010219     C                   ENDIF
025400010219     C                   IF        %trim(%subst(vindta:377:3)) = 'BPC'
025500010219     C                   Z-ADD     0591255       VABCCM
025600010219     C                   ENDIF
025700010219     C*
025800010219     C                   EVAL      PiStr=%trim(%subst(vindta:384:6))
025900010219     C                   EXSR      CHKNUM
026000010219     C                   IF        PiInt=*on
026100010219     C                   Z-ADD     PiVal         VABNSP
026200010219     C                   Z-ADD     PiVal         VABRMN
026300010219     C                   ELSE
026400010219     C                   ADD       1             errore
026500010219     C                   EVAL      VABRMA = %trim(PiStr)
026600010219     C                   EVAL      vinmsg = %trimr(vinmsg)
026700010219     C                             + ' ' + 'VABNSP'
026800010219     C                   ENDIF
026900010215     C*
027000010219     C* Reperimento campi NUMERICI e/o CALCOLATI
027100010219     C                   EVAL      PiStr=%trim(%subst(vindta:872:3))
027200010201     C                   EXSR      CHKNUM
027300010201     C                   IF        PiInt=*on
027400010219     C                   Z-ADD     PiVal         VABNCL
027500010201     C                   ENDIF
027600010215     C*
027700010219     C                   EVAL      PiStr=%trim(%subst(vindta:875:7))
027800010201     C                   EXSR      CHKNUM
027900010201     C                   IF        PiNum=*on
028000010219     C                   EVAL      VABPKB = PiVal/100
028100010201     C                   ENDIF
028200010215     C*
028300010219     C                   EVAL      PiStr=%trim(%subst(vindta:889:8))
028400010215     C                   EXSR      CHKNUM
028500010215     C                   IF        PiNum=*on
028600010219     C                   EVAL      VABVLB = PiVal/1000
028700010215     C                   ENDIF
028800010219     C*
028900010215     C                   EVAL      VABCBO = '1'
029000010202     C*
029100000801     C* Ebbene...
029200000801     C                   ADD       1             §CTRMO
029300010201     C                   IF        errore <> *zeros
029400000801     C                   ADD       1             §CTRNO
029500000801     C                   EVAL      recko = vindta
029600000801     C                   EXCEPT    rigadett
029700000801     C                   EVAL      vinflg = '2'
029800000801     C                   ELSE
029900010215     C                   ADD       1             §CTROK
030000000801     C                   ENDIF
030100000801     C*
030200000801     C                   ENDSR
030300000801     C*----------------------------------------------------*
030400000801     C*  CONTROLLO NUMERICITA' CAMPI
030500000801     C*----------------------------------------------------*
030600000801     C     CHKNUM        BEGSR
030700000801     C*
030800000801     C                   call(e)   'ISNUMERIC'
030900000801     C                   PARM                    PiStr            30
031000000801     C                   PARM      ','           PiDecChr          1
031100000801     C                   PARM      *ZEROS        PiVal            30 9
031200000801     C                   PARM      '0'           PiInt             1
031300000801     C                   PARM      '0'           PiNum             1
031400000801     C                   IF        %error
031500000801     C                   EVAL      PiInt=*off
031600000801     C                   ENDIF
031700000801     C*
031800000801     C                   ENDSR
031900000801     C***
032000000801
032100000801
032200000801
032300990920      /TITLE Invio dei dati al punto operativo.
032400010202     C     invio         BEGSR
032500010201     C*
032600021113     C* Invio FIVAB
032700010201     C                   reset                   dscmz
032800010201     C                   move      vlrpoi        cmzdst
032900010201     C                   eval      cmzfld = vlrfou
033000010201     C                   eval      cmzmbd = vlrhdl
033100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
033200010201     C                   if        prmfir = *blanks
033300021113     C                   eval      cmzfla = 'FIVAB00F'
033400021113     C                   eval      cmzmba = 'FIVAB00F'
033500010201     C                   else
033600010201     C                   eval      cmzfla = prmfir
033700010201     C                   eval      cmzmba = prmfir
033800010201     C                   endif
033900010201     C                   eval      cmznrr = *zeros
034000010215     C                   move      §ctrok        cmznrr
034100021025     C                   eval      cmzlba = vlrfl1
034200010201     C                   call(e)   'TIS711C'
034300010201     C                   parm                    dscmz
034400010201     C                   parm      *blanks       esito
034500010201     C                   if        %error
034600010201     C                             or cmzerr = '1'
034700010201     C                             or esito  = '1'
034800010201     C                   eval      wrkesito = '3'
034900010205     C                   endif
035000990920     C*
035100000613     C                   ENDSR
035200000613     C***
035300990910
035400000613     C     *inzsr        BEGSR
035500990910     C*
035600990910     C     *entry        plist
035700990920     C                   parm                    tivlrds
035800990921     C                   parm      wrkesito      esito
035900000724     C                   parm                    prmlit
036000000710     C                   parm                    prmfir
036100000613     C*
036200000830     C* CALCOLA LA DATA CORRENTE
036300000830     C                   time                    wn14             14 0
036400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
036500000830     C                   z-add     wn8           g08dat
036600000830     C                   z-add     *zeros        g08inv
036700000830     C                   movel     '0'           g08err
036800000830     C                   call      'XSRDA8'
036900000830     C                   parm                    wlbda8
037000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
037100000830     C*
037200000613     C                   ENDSR
037300000613     C***
037400990908
037500010219     Otis7nott  E            riepilogo         2
037600990915     O                                              'Upload via Internet'
037700990915     O                                           +1 'Traduzione TIVIN00R -'
037800021113     O                                           +1 'FIVABWWR'
037900010201     O                                           +1 'Report testate bolle'
038000990915     O          E            riepilogo   2
038100990915     O                       wrkdata
038200990915     O                       wrkora              +1
038300990915     O                       procname            +1
038400990915     O          E            riepilogo   2
038500990915     O                                              'Cliente..................:'
038600990915     O                       VABCCM        z     +1
038700990915     O          E            riepilogo   2
038800990920     O                                              'Riferimento Strategi.....:'
038900990920     O                       vlrhdl              +1
039000990915     O          E            riepilogo   2
039100990915     O                                              'Giusti...................:'
039200010215     O                       §CTROK        2   +  1
039300990915     O          E            riepilogo   2
039400990915     O                                              'Sbagliati e corretti.....:'
039500971022     O                       §CTRMO        2   +  1
039600990915     O          E            riepilogo   2
039700990915     O                                              'Sbagliati e scartati.....:'
039800971022     O                       §CTRNO        2   +  1
039900000613
040000010219     Otis7notts E            testdett          2
040100000613     O                                              'Upload via Internet'
040200000613     O                                           +1 'Traduzione TIVIN00R -'
040300021113     O                                           +1 'FIVABWWR'
040400010201     O                                           +1 'Report testate bolle'
040500000616     O          E            testdett    3
040600000613     O                                           +2 'N° rec'
040700000613     O                                           +3 'Anteprima contenuto'
040800000616     O          E            rigadett    2
040900000613     O                       rrnum               +2
041000000621     O                       recko               +3
041100000616     O          E            findett     2
041200000613     O                       wrkdata
041300000613     O                       wrkora              +1
041400000613     O                       procname            +1
041500000616     O          E            findett     2
041600000613     O                                              'Cliente..................:'
041700000613     O                       VABCCM        z     +1
041800000616     O          E            findett     2
041900000613     O                                              'Riferimento Strategi.....:'
042000000613     O                       vlrhdl              +1
042100000616     O          E            findett     2
042200000613     O                                              'Giusti...................:'
042300010215     O                       §CTROK        2   +  1
042400000616     O          E            findett     2
042500000613     O                                              'Sbagliati e corretti.....:'
042600000613     O                       §CTRMO        2   +  1
042700000616     O          E            findett     2
042800000613     O                                              'Sbagliati e scartati.....:'
042900000613     O                       §CTRNO        2   +  1
043000000512** CMD - COMANDI CL
043100011113OVRPRTF FILE(TIS7NOTT) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
043200011113OVRPRTF FILE(TIS7NOTTS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
043300010219DLTOVR FILE(TIS7NOTT TIS7NOTTS) LVL(*)
043400000512
043500000512
