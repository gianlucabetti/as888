000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800071214     Ftitv1d2p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100071214     Ftitv1d2ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600060725     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000071214     D wNumColloDA     s              7  0 INZ(*zeros)
004100071214     D wNumColloA      s              7  0 INZ(*zeros)
004200000830
004300000830     D*------------------
004400000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500000830     D*------------------
004600000830     D WLBDA8          DS                  INZ
004700000830     D  G08DAT                 1      8  0
004800000830     D  G08INV                 9     16  0
004900000830     D  G08ERR                17     17
005000000830     D  G08TGI                18     22  0
005100000830     D*
005200990908
005300060307     D*------------------
005400060307     D* DS REPERIMENTO NUMERATORE
005500060307     D*------------------
005600060529     D trul33ds      e ds                  inz
005700060307     D*------------------
005800060307     D* DS ARCHITETTURA
005900060307     D*------------------
006000060307     D kpjba         e ds                  inz
006100060307     D*------------------
006200010201
006300010201
006400990915     C                   time                    wrkdata
006500990915     C                   time                    wrkora
006600000913     C                   reset                   rrnum
006700990921     C                   reset                   esito
006800990921     C                   reset                   wrkesito
006900000724     C*
007000000724     C* SE OCCORRE SPEDIRE IN FILIALE
007100000724     C                   if        vlrpoi <> *zeros
007200000724     C*
007300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007500000724     C     vlrpoi        chain     azorg01l
007600000724     C                   if        %found
007700000616     C                   movel(p)  CMD(1)        depcmd
007800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007900000616     C*
008000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008100011113     C                   Z-ADD     150           LENGH            15 5
008200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008300000616     C                   PARM                    depcmd
008400000616     C                   PARM                    LENGH
008500000724     C*
008600000724     C                   endif
008700000724     C                   endif
008800000616     C*
008900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009000000616     C                   movel(p)  CMD(2)        depcmd
009100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009200000616     C*
009300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009400011113     C                   Z-ADD     150           LENGH            15 5
009500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009600000616     C                   PARM                    depcmd
009700000616     C                   PARM                    LENGH
009800000616     C*
009900071214     C                   if        not %open(titv1d2ps)
010000071214     C                   open      titv1d2ps
010100000616     C                   except    testdett
010200000613     C                   endif
010300000613     C*
010400040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010600000613     C*
010700010202     C* Effettuo la chiamata al CLLE preposto
010800040506     C                   call(e)   'TITVVTC'
010900010202     C                   parm                    parccm
011000010202     C                   parm                    parmbr
011100010202     C                   parm      '2'           paropz
011200050201     C*
011300050201     C* Effettuo lancio TISI95 solo x chiusura
011400050201     C                   CLEAR                   TISI95DS
011500050201     C                   EVAL      I95TLA = 'C'
011600050201     C                   CALL      'TISI95R'
011700050201     C                   PARM                    TISI95DS
011800010202     C*
011900071214     C                   if        %open(titv1d2ps)
012000000616     C                   except    findett
012100071214     C                   close     titv1d2ps
012200000613     C                   endif
012300000616     C*
012400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012500011113     C                   Z-ADD     150           LENGH            15 5
012600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012700000616     C                   PARM                    CMD(3)
012800000616     C                   PARM                    LENGH
012900000616     C*
013000000801     C
013100010201     C                   seton                                        LR
013200000613
013300000613
013400000613     C*--------------------------------------------------------
013500000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013600000613     C*--------------------------------------------------------
013700000621     C     STPR          BEGSR
013800000613     C*
013900071214     C                   if        not %open(titv1d2p)
014000071214     C                   open      titv1d2p
014100990915     C                   endif
014200990915     C*
014300990915     C                   except    riepilogo
014400990915     C*
014500071214     C                   if        %open(titv1d2p)
014600071214     C                   close     titv1d2p
014700990914     C                   endif
014800990910     C*
014900000613     C                   ENDSR
015000000613     C***
015100990908
015200000801
015300910830     C*--------------------------------------------------------
015400040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015500910830     C*--------------------------------------------------------
015600040526     C     RWFILE        BEGSR
015700990910     C*
015800990914     C                   if        not %open(tivin00r)
015900990908     C                   open      tivin00r
016000990914     C                   endif
016100021113     C                   if        not %open(fivabwwr)
016200021113     C                   open      fivabwwr
016300990914     C                   endif
016400021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016500020305     C                   exsr      prevat
016600010201     C*
016700010202     C                   if        chkcall = '0'
016800010202     C*
016900021113     C                   if        not %open(fivatwwr)
017000021113     C                   open      fivatwwr
017100010201     C                   endif
017200990910     C*
017300010201     C                   clear                   §CTROKVB          5 0
017400020305     C                   clear                   §CTROKVT          5 0
017500000801     C                   clear                   §CTRMO            5 0
017600000801     C                   clear                   §CTRNO            5 0
017700990910     C*
017800921023     C                   DO        *HIVAL
017900990913     C*
018000990915     C                   READ      tivin00r                               70
018100050627     C                   if        vindta > *blanks
018200000613     C                   add       1             rrnum
018300000801     C*
018400000801     C                   if        *in70 = *off
018500000801     C                             and
018600000801     C                             (vinflg = *blanks
018700000801     C                              or vinflg = '0'
018800000801     C                              or vinflg = '2')
018900000801     C*
019000000801     C                   clear                   vinmsg
019100000801     C                   eval      vinflg = '1'
019200010305     C*
019300050628     C                   clear                   fivab000
019400060725     C                   clear                   fivat000
019500050628     C                   exsr      impvab                                       => carico VAB
019600060725     C                   exsr      wrivab                                       => scarico VAB
019700000905     C*
019800000905     C                   else
019900000905     C                   eval      vinflg = '1'
020000050628     C                   endif
020100000905     C                   endif
020200000905     C*
020300000905     C  N70              update    tivin000
020400000905     C*
020500991022     C  N70              ENDdo
020600010202     C*
020700010202     C                   endif
020800990910
020900990910     C* Se non ci sono record con errori ...
021000000710     C                   if        §ctrno = 0
021100990910     C* ... restituisco esito OK.
021200990921     C                   eval      wrkesito = '0'
021300990910     C                   else
021400010201     C                   if        §ctrokvb > 0
021500990921     C                   eval      wrkesito = '1'
021600000710     C                   else
021700000710     C                   eval      wrkesito = '2'
021800990910     C                   endif
021900000710     C                   endif
022000990910     C*
022100990914     C                   if        %open(tivin00r)
022200990908     C                   close     tivin00r
022300990914     C                   endif
022400021113     C                   if        %open(fivabwwr)
022500021113     C                   close     fivabwwr
022600990914     C                   endif
022700021113     C                   if        %open(fivatwwr)
022800021113     C                   close     fivatwwr
022900010201     C                   endif
023000990910     C*
023100010201     C                   if        §ctrokvb > 0
023200000724     C                             and vlrpoi <> *zeros
023300010202     C                   exsr      invio
023400990920     C                   endif
023500990920     C*
023600910830     C                   ENDSR
023700000613     C***
023800010305
023900010305     C*----------------------------------------------------*
024000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
024100010305     C*----------------------------------------------------*
024200020305     C     WRIVAB        BEGSR
024300010305     C*
024400060225     C* Quindi scarico il buffer del file d testata
024500021113     C                   write     fivab000                                     => scarico il VAB
024600010305     C*
024700010305     C                   ENDSR
024800990920
024900000801     C*----------------------------------------------------*
025000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025100000801     C*----------------------------------------------------*
025200010201     C     INZVAR        BEGSR
025300000801     C*
025400040802     C                   Z-ADD     *zeros        Num5_0            5 0
025500040802     C                   MOVEL     '0'           FlgCAS            1
025600000801     C*
025700000801     C                   ENDSR
025800000801     C*----------------------------------------------------*
025900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026000000801     C*----------------------------------------------------*
026100000801     C     DEFCAM        BEGSR
026200000801     C*
026300020619     C* Imposto i valori di default...
026400071214     C                   Z-ADD     0894535       VABCCM
026500071214     C                   Z-ADD     0894535       VATCCM
026600071214     C                   Z-ADD     089           VABLNP
026700071214     C                   Z-ADD     089           VATLNP
026800040714     C                   Z-ADD     000           VABCTR
026900071214     C                   MOVEL     'C'           VABTSP
027000060529     C                   MOVEL     '7Q'          VABCTM
027100060725     C                   MOVEL     '1'           VABCBO
027200020619     C* ... e poi verifico se sono stati passati come parametri
027300020619     C                   IF        vlrppt > *blanks
027400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
027500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
027600020619     C                   EXSR      CHKNUM
027700020619     C                   IF        PiInt=*on
027800020619     C                   Z-ADD     PiVal         VABCCM
027900020619     C                   Z-ADD     PiVal         VATCCM
028000020619     C                   ENDIF
028100040506     C                   ENDIF
028200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
028300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
028400020619     C                   EXSR      CHKNUM
028500020619     C                   IF        PiInt=*on
028600020619     C                   Z-ADD     PiVal         VABLNP
028700020619     C                   Z-ADD     PiVal         VATLNP
028800040506     C                   ENDIF
028900020619     C                   ENDIF
029000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
029100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029200020619     C                   EXSR      CHKNUM
029300020619     C                   IF        PiInt=*on
029400020619     C                   Z-ADD     PiVal         VABCTR
029500040506     C                   ENDIF
029600020619     C                   ENDIF
029700020619     C                   ENDIF
029800000801     C*
029900000801     C                   ENDSR
030000000801     C*----------------------------------------------------*
030100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
030200000801     C*----------------------------------------------------*
030300040823     C     IMPVAB        BEGSR
030400040823     C*
030500020305     C                   EXSR      INZVAR
030600020305     C                   EXSR      DEFCAM
030700010305     C*
030800000801     C                   Z-ADD     *zeros        errore            1 0
030900000830     C                   MOVEL     datcor        VABAAS
031000020305     C                   MOVEL     datcor        VATAAS
031100040526     C                   MOVE      datcor        VABMGS
031200060529     C                   MOVE(P)   vlrpoi        VABFGS
031300060529     C                   MOVE(P)   vlrpoi        VATFGS
031400050628     C*
031500071214     C                   EVAL      VABRSD=%trim(%subst(vindta:243:30))
031600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
031800020117     C* ==
031900071214     C                   EVAL      VABIND=%trim(%subst(vindta:273:30))
032000071214     C                   EVAL      VABLOD=%trim(%subst(vindta:303:30))
032100071214     C                   EVAL      VABPRD=%trim(%subst(vindta:333:5))
032200071214     C                   EVAL      VABNOT=%trim(%subst(vindta:550:30))
032300071214     C                   EVAL      VABNT2=%trim(%subst(vindta:580:30))
032400071214     C                   EVAL      VABRMA=%trim(%subst(vindta:163:15))
032500071214     C* TCR
032600071214     C                   IF        %trim(%subst(vindta:533:1)) = 'E'
032700071214     C                   EVAL      VABTCR='P'
032800071214     C                   ENDIF
032900071214     C                   IF        %trim(%subst(vindta:533:1)) = 'D'
033000071214     C                   EVAL      VABTCR='D'
033100071214     C                   ENDIF
033200071214     C                   IF        %trim(%subst(vindta:533:1)) = 'T' OR
033300071214     C                             %trim(%subst(vindta:533:1)) = 'C'
033400071214     C                   EVAL      VABTCR=*blanks
033500071214     C                   ENDIF
033600071214     C* GC1 - 1°
033700071214     C                   IF        %trim(%subst(vindta:534:3)) = 'LUN'
033800071214     C                   EVAL      %subst(VABGC1:1:1)='1'
033900071214     C                   ENDIF
034000071214     C                   IF        %trim(%subst(vindta:534:3)) = 'MAR'
034100071214     C                   EVAL      %subst(VABGC1:1:1)='2'
034200071214     C                   ENDIF
034300071214     C                   IF        %trim(%subst(vindta:534:3)) = 'MER'
034400071214     C                   EVAL      %subst(VABGC1:1:1)='3'
034500071214     C                   ENDIF
034600071214     C                   IF        %trim(%subst(vindta:534:3)) = 'GIO'
034700071214     C                   EVAL      %subst(VABGC1:1:1)='4'
034800071214     C                   ENDIF
034900071214     C                   IF        %trim(%subst(vindta:534:3)) = 'VEN'
035000071214     C                   EVAL      %subst(VABGC1:1:1)='5'
035100071214     C                   ENDIF
035200071214     C                   IF        %trim(%subst(vindta:534:3)) = 'SAB'
035300071214     C                   EVAL      %subst(VABGC1:1:1)='6'
035400071214     C                   ENDIF
035500071214     C                   IF        %trim(%subst(vindta:534:3)) = 'DOM'
035600071214     C                   EVAL      %subst(VABGC1:1:1)='7'
035700071214     C                   ENDIF
035800071214     C* GC1 - 2°
035900071214     C                   IF        %trim(%subst(vindta:537:1)) = 'M'
036000071214     C                   EVAL      %subst(VABGC1:2:1)='M'
036100071214     C                   ENDIF
036200071214     C                   IF        %trim(%subst(vindta:537:1)) = 'P'
036300071214     C                   EVAL      %subst(VABGC1:2:1)='P'
036400071214     C                   ENDIF
036500071214     C                   IF        %trim(%subst(vindta:537:1)) = 'E'
036600071214     C                   EVAL      %subst(VABGC1:2:1)=*blanks
036700071214     C                   ENDIF
036800071214     C* GC2 - 1°
036900071214     C                   IF        %trim(%subst(vindta:538:3)) = 'LUN'
037000071214     C                   EVAL      %subst(VABGC2:1:1)='1'
037100071214     C                   ENDIF
037200071214     C                   IF        %trim(%subst(vindta:538:3)) = 'MAR'
037300071214     C                   EVAL      %subst(VABGC2:1:1)='2'
037400071214     C                   ENDIF
037500071214     C                   IF        %trim(%subst(vindta:538:3)) = 'MER'
037600071214     C                   EVAL      %subst(VABGC2:1:1)='3'
037700071214     C                   ENDIF
037800071214     C                   IF        %trim(%subst(vindta:538:3)) = 'GIO'
037900071214     C                   EVAL      %subst(VABGC2:1:1)='4'
038000071214     C                   ENDIF
038100071214     C                   IF        %trim(%subst(vindta:538:3)) = 'VEN'
038200071214     C                   EVAL      %subst(VABGC2:1:1)='5'
038300071214     C                   ENDIF
038400071214     C                   IF        %trim(%subst(vindta:538:3)) = 'SAB'
038500071214     C                   EVAL      %subst(VABGC2:1:1)='6'
038600071214     C                   ENDIF
038700071214     C                   IF        %trim(%subst(vindta:538:3)) = 'DOM'
038800071214     C                   EVAL      %subst(VABGC2:1:1)='7'
038900071214     C                   ENDIF
039000071214     C* GC2 - 2°
039100071214     C                   IF        %trim(%subst(vindta:541:1)) = 'M'
039200071214     C                   EVAL      %subst(VABGC2:2:1)='M'
039300071214     C                   ENDIF
039400071214     C                   IF        %trim(%subst(vindta:541:1)) = 'P'
039500071214     C                   EVAL      %subst(VABGC2:2:1)='P'
039600071214     C                   ENDIF
039700071214     C                   IF        %trim(%subst(vindta:541:1)) = 'E'
039800071214     C                   EVAL      %subst(VABGC2:2:1)=*blanks
039900071214     C                   ENDIF
040000071214     C* DCR
040100071214     C                   EVAL      PiStr=%trim(%subst(vindta:517:8))
040200071214     C                   EXSR      CHKNUM
040300071214     C                   IF        PiInt=*on
040400071214     C                   Z-ADD     PiVal         VABDCR
040500071214     C                   ELSE
040600071214     C                   ADD       1             errore
040700071214     C                   EVAL      vinmsg = %trimr(vinmsg)
040800071214     C                             + ' ' + 'VABDCR'
040900071214     C                   ENDIF
041000060529     C* RMN
041100071214     C                   EVAL      PiStr=%trim(%subst(vindta:1:14))
041200060529     C                   EXSR      CHKNUM
041300060529     C                   IF        PiInt=*on
041400060529     C                   Z-ADD     PiVal         VABRMN
041500060529     C                   ELSE
041600060529     C                   ADD       1             errore
041700060529     C                   EVAL      vinmsg = %trimr(vinmsg)
041800060529     C                             + ' ' + 'VABRMN'
041900060529     C                   ENDIF
042000060529     C* NSP => Stacco un numeratore da AZNUM
042100060529     C                   clear                   TRUL33DS
042200060529     C                   eval      I33OPE = *zeros
042300060529     C                   eval      I33CNU = 302
042400060529     C                   eval      I33NUM = 1
042500060529     C                   movel     TRUL33DS      KPJBU
042600060529     C                   call      'TRUL33R'
042700060529     C                   parm                    KPJBA
042800060529     C                   movel     KPJBU         TRUL33DS
042900060529     C                   if        O33ERR = *zeros
043000060529     C                   z-add     O33NRF        VABNSP
043100060529     C                   z-add     O33NRF        VATNSP
043200060529     C                   else
043300060529     C                   Z-ADD     1             errore
043400060529     C                   EVAL      vinmsg = %trimr(vinmsg)
043500060529     C                             + ' ' + 'VABNSP VATNSP'
043600060529     C                   endif
043700060529     C* CAD
043800071214     C                   EVAL      PiStr=%trim(%subst(vindta:343:10))
043900010201     C                   EXSR      CHKNUM
044000010201     C                   IF        PiInt=*on
044100010201     C                   Z-ADD     PiVal         Num5_0
044200040506     C                   MOVEL(P)  Num5_0        VABCAD
044300010201     C                   ELSE
044400040506     C                   ADD       1             errore
044500010201     C                   EVAL      vinmsg = %trimr(vinmsg)
044600040506     C                             + ' ' + 'VABCAD'
044700010201     C                   ENDIF
044800040506     C* Reperisco la provincia dal CAP e dalla località
044900040526     C                   IF        VABCAD <> *blanks AND
045000040526     C                             VABPRD  = *blanks
045100040506     C                   CLEAR                   TISI95DS
045200040506     C                   EVAL      I95TCN = '3'
045300040506     C                   Z-ADD     datcor        I95DAT
045400040506     C                   EVAL      I95CAP = VABCAD
045500040506     C                   EVAL      I95LOC = VABLOD
045600050627     C                   EVAL      I95NAR = VABNZD
045700040506     C                   CALL      'TISI95R'
045800040506     C                   PARM                    TISI95DS
045900040506     C                   EVAL      VABPRD = O95PRV
046000040506     C                   ENDIF
046100071214     C* NCL 1
046200071214     C                   EVAL      PiStr=%trim(%subst(vindta:502:5))
046300060725     C                   EXSR      CHKNUM
046400060725     C                   IF        PiInt=*on
046500071214     C                   ADD       PiVal         VABNCL
046600060725     C                   ELSE
046700060725     C                   ADD       1             errore
046800060725     C                   EVAL      vinmsg = %trimr(vinmsg)
046900060725     C                             + ' ' + 'VABNCL'
047000060725     C                   ENDIF
047100071214     C* NCL 2
047200071214     C                   EVAL      PiStr=%trim(%subst(vindta:507:3))
047300071214     C                   EXSR      CHKNUM
047400071214     C                   IF        PiInt=*on
047500071214     C                   ADD       PiVal         VABNCL
047600071214     C                   ELSE
047700071214     C                   ADD       1             errore
047800071214     C                   EVAL      vinmsg = %trimr(vinmsg)
047900071214     C                             + ' ' + 'VABNCL'
048000071214     C                   ENDIF
048100040506     C* PKB
048200071214     C                   EVAL      PiStr=%trim(%subst(vindta:378:8))
048300010201     C                   EXSR      CHKNUM
048400010201     C                   IF        PiNum=*on
048500071207     C                   EVAL      PiVal=PiVal / 1000                           * gestisco 3 dec.
048600060725     C                   Z-ADD(H)  PiVal         VABPKB
048700010201     C                   ELSE
048800010201     C                   ADD       1             errore
048900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
049000010201     C                             + ' ' + 'VABPKB'
049100010201     C                   ENDIF
049200060725     C* CAS
049300071214     C                   IF        %subst(vindta:413:13)<>*all'0'
049400071207     C                   EVAL      FlgCAS='1'
049500071214     C                   EVAL      VABVCA=%trim(%subst(vindta:426:3))
049600071214     C                   EVAL      PiStr=%trim(%subst(vindta:413:13))
049700060307     C                   EXSR      CHKNUM
049800060307     C                   IF        PiNum=*on
049900071214     C                   EVAL      PiVal=PiVal / 10000                          * gestisco 4 dec.
050000060725     C                   Z-ADD     PiVal         VABCAS
050100060307     C                   ELSE
050200060307     C                   ADD       1             errore
050300060307     C                   EVAL      vinmsg = %trimr(vinmsg)
050400060307     C                             + ' ' + 'VABCAS'
050500060307     C                   ENDIF
050600060725     C                   ENDIF
050700060529     C*
050800060529     C* Gestione Disk C
050900060725     C                   exsr      exeVATE
051000010205     C*
051100010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
051200040802     C                   IF        FlgCAS <> '0'
051300071207     C                   IF        VABCBO = '1'
051400071207     C                   EVAL      VABCBO = '4'
051500071207     C                   ENDIF
051600071207     C                   IF        VABCBO = '2'
051700071207     C                   EVAL      VABCBO = '6'
051800071207     C                   ENDIF
051900071207     C                   ENDIF
052000020305     C*
052100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052200011113     C                   EXSR      CHKIMPDIV
052300010202     C*
052400000801     C* Ebbene...
052500000801     C                   ADD       1             §CTRMO
052600010201     C                   IF        errore <> *zeros
052700000801     C                   ADD       1             §CTRNO
052800000801     C                   EVAL      recko = vindta
052900000801     C                   EXCEPT    rigadett
053000000801     C                   EVAL      vinflg = '2'
053100000801     C                   ELSE
053200010201     C                   ADD       1             §CTROKVB
053300000801     C                   ENDIF
053400000801     C*
053500000801     C                   ENDSR
053600050628     C*----------------------------------------------------*
053700060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
053800050628     C*----------------------------------------------------*
053900071207     C     EXEVATE       BEGSR
054000071207     C*
054100071207     C* Inizializzo progressivo collo su totale colli della spedizione
054200071207     C                   Z-ADD     *zeros        wNumColloDA
054300071207     C                   Z-ADD     *zeros        wNumColloA
054400071207     C* Collo "DA"
054500071214     C                   EVAL      PiStr=%trim(%subst(vindta:394:7))
054600071207     C                   EXSR      CHKNUM
054700071207     C                   IF        PiInt=*on
054800071207     C                   Z-ADD     PiVal         wNumColloDA
054900071207     C                   ELSE
055000071207     C                   ADD       1             errore
055100071207     C                   EVAL      vinmsg = %trimr(vinmsg)
055200071207     C                             + ' ' + 'VATNOT_E'
055300071207     C                   ENDIF
055400071207     C* Collo "A"
055500071214     C                   EVAL      PiStr=%trim(%subst(vindta:401:7))
055600071207     C                   EXSR      CHKNUM
055700071207     C                   IF        PiInt=*on
055800071207     C                   Z-ADD     PiVal         wNumColloA
055900071207     C                   ELSE
056000071207     C                   ADD       1             errore
056100071207     C                   EVAL      vinmsg = %trimr(vinmsg)
056200071207     C                             + ' ' + 'VATNOT_E'
056300071207     C                   ENDIF
056400071207     C*
056500071207     C* Costriuisco <N> barcode a seconda del numero d colli presenti sulla bolla
056600071207     C                   EVAL      VATTRC = 'E'
056700071207     C                   DOW       wNumColloDA <= wNumColloA
056800071207     C                   MOVEL(P)  wNumColloDA   VATNOT
056900071207     C                   EVAL      wNumColloDA = wNumColloDA + 1
057000071207     C                   write     FIVAT000
057100071207     C                   add       1             §CTROKVT
057200071207     C                   ENDDO
057300050628     C*
057400050628     C                   ENDSR
057500010202     C*----------------------------------------------------*
057600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
057700010202     C*----------------------------------------------------*
057800020305     C     PREVAT        BEGSR
057900010202     C*
058000021113     C* Compongo il nome del membro da dare al FIVATWWR
058100010202     C                   eval      parmbr = vlrhdl
058200010202     C                   movel     'M'           parmbr
058300050627     C                   eval      parccm = %subst(vlrKSC:2:7)
058400010202     C                   eval      paropz = '1'
058500010202     C* Effettuo la chiamata al CLLE preposto
058600040506     C                   call(e)   'TITVVTC'
058700010202     C                   parm                    parccm
058800010202     C                   parm                    parmbr
058900010202     C                   parm                    paropz
059000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
059100010202     C                   if        %error
059200010202     C                   movel     '1'           chkcall
059300010202     C                   else
059400010202     C                   movel     '0'           chkcall
059500010202     C                   endif
059600010202     C*
059700010202     C                   ENDSR
059800000801     C*----------------------------------------------------*
059900000801     C*  CONTROLLO NUMERICITA' CAMPI
060000000801     C*----------------------------------------------------*
060100000801     C     CHKNUM        BEGSR
060200000801     C*
060300000801     C                   call(e)   'ISNUMERIC'
060400000801     C                   PARM                    PiStr            30
060500060725     C                   PARM      '.'           PiDecChr          1
060600000801     C                   PARM      *ZEROS        PiVal            30 9
060700000801     C                   PARM      '0'           PiInt             1
060800000801     C                   PARM      '0'           PiNum             1
060900000801     C                   IF        %error
061000000801     C                   EVAL      PiInt=*off
061100000801     C                   ENDIF
061200000801     C*
061300000801     C                   ENDSR
061400000801     C***
061500000801
061600011113
061700011113     C*----------------------------------------------------*
061800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
061900011113     C*----------------------------------------------------*
062000011113     C     CHKIMPDIV     BEGSR
062100011113     C*
062200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
062300011113     C                   Z-ADD     *zeros        wrkDec            9 9
062400011113     C*
062500011113     C* Come prima cosa effettuo considerazioni sulla divisa
062600011113     C                   IF        vabIAS > *zeros
062700011113     C                   IF        vabVAS <> 'EUR'
062800011113     C                   EVAL      vabVAS =  'ITL'
062900011113     C                   ENDIF
063000011113     C                   ENDIF
063100011113     C*
063200011113     C                   IF        vabCAS > *zeros
063300011113     C                   IF        vabVCA <> 'EUR'
063400011113     C                   EVAL      vabVCA =  'ITL'
063500011113     C                   ENDIF
063600011113     C                   ENDIF
063700011113     C*
063800011113     C                   IF        vabVMD > *zeros
063900020305     C                   IF        vabVAD <> 'EUR'
064000011113     C                   EVAL      vabVAD =  'ITL'
064100011113     C                   ENDIF
064200011113     C                   ENDIF
064300011113     C*
064400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
064500011113     C                   Z-ADD     vabIAS        wrkDec
064600011113     C                   IF        wrkDec > *zeros
064700011113     C                   IF        vabVAS = 'ITL'
064800011113     C                   EVAL      vabIAS = *zeros
064900011113     C                   ENDIF
065000011113     C                   ENDIF
065100011113     C*
065200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
065300011113     C                   Z-ADD     vabCAS        wrkDec
065400011113     C                   IF        wrkDec > *zeros
065500011113     C                   IF        vabVCA = 'ITL'
065600011113     C                   EVAL      vabCAS = *zeros
065700011113     C                   ENDIF
065800011113     C                   ENDIF
065900011113     C*
066000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
066100011113     C                   Z-ADD     vabVMD        wrkDec
066200011113     C                   IF        wrkDec > *zeros
066300011113     C                   IF        vabVAD = 'ITL'
066400011113     C                   EVAL      vabVMD = *zeros
066500011113     C                   ENDIF
066600011113     C                   ENDIF
066700011113     C*
066800011113     C                   ENDSR
066900011113     C***
067000011113
067100011113
067200000801
067300000801
067400990920      /TITLE Invio dei dati al punto operativo.
067500010202     C     invio         BEGSR
067600990920     C*
067700021113     C* 1° invio FIVAT
067800010201     C                   reset                   dscmz
067900010201     C                   move      vlrpoi        cmzdst
068000021113     C                   eval      cmzfld = 'FIVATWWR'
068100010201     C                   eval      cmzmbd = vlrhdl
068200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068300021009     C***                if        prmfir = *blanks
068400021113     C                   eval      cmzfla = 'FIVAT00F'
068500021113     C                   eval      cmzmba = 'FIVAT00F'
068600021009     C***                else
068700021009     C***                eval      cmzfla = prmfir
068800021009     C***                eval      cmzmba = prmfir
068900021009     C***                endif
069000010201     C                   eval      cmznrr = *zeros
069100020305     C                   move      §ctrokvt      cmznrr
069200021018     C                   eval      cmzlba = vlrfl1
069300010201     C                   call(e)   'TIS711C'
069400010201     C                   parm                    dscmz
069500010201     C                   parm      *blanks       esito
069600010205     C                   if        %error
069700010205     C                             or cmzerr = '1'
069800010205     C                             or esito  = '1'
069900010205     C                   eval      wrkesito = '3'
070000010205     C                   else
070100010201     C*
070200021113     C* 2° invio FIVAB
070300010201     C                   reset                   dscmz
070400010201     C                   move      vlrpoi        cmzdst
070500010201     C                   eval      cmzfld = vlrfou
070600010201     C                   eval      cmzmbd = vlrhdl
070700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070800021009     C***                if        prmfir = *blanks
070900021113     C                   eval      cmzfla = 'FIVAB00F'
071000021113     C                   eval      cmzmba = 'FIVAB00F'
071100021009     C***                else
071200021009     C***                eval      cmzfla = prmfir
071300021009     C***                eval      cmzmba = prmfir
071400021009     C***                endif
071500010201     C                   eval      cmznrr = *zeros
071600010201     C                   move      §ctrokvb      cmznrr
071700021018     C                   eval      cmzlba = vlrfl1
071800010201     C                   call(e)   'TIS711C'
071900010201     C                   parm                    dscmz
072000010201     C                   parm      *blanks       esito
072100010201     C                   if        %error
072200010201     C                             or cmzerr = '1'
072300010201     C                             or esito  = '1'
072400010201     C                   eval      wrkesito = '3'
072500010201     C                   endif
072600010205     C                   endif
072700990920     C*
072800000613     C                   ENDSR
072900000613     C***
073000070411
073100070411     C     *pssr         BEGSR
073200070411     C*
073300070411     C                   if        %open(tivin00r)
073400070411     C                   close     tivin00r
073500070411     C                   endif
073600070411     C                   if        %open(fivabwwr)
073700070411     C                   close     fivabwwr
073800070411     C                   endif
073900070411     C                   if        %open(fivatwwr)
074000070411     C                   close     fivatwwr
074100070411     C                   endif
074200070411     C*
074300070411     C* Effettuo la chiamata al CLLE preposto
074400070411     C                   call(e)   'TITVVTC'
074500070411     C                   parm                    parccm
074600070411     C                   parm                    parmbr
074700070411     C                   parm      '2'           paropz
074800070411     C*
074900070411     C                   eval      wrkesito = '2'
075000070411     C*
075100070411     C                   seton                                        LR
075200070411     C*
075300070411     C                   ENDSR     '*CANCL'
075400070411     C***
075500070411
075600990910
075700000613     C     *inzsr        BEGSR
075800990910     C*
075900990910     C     *entry        plist
076000990920     C                   parm                    tivlrds
076100990921     C                   parm      wrkesito      esito
076200000724     C                   parm                    prmlit
076300000710     C                   parm                    prmfir
076400000613     C*
076500000830     C* CALCOLA LA DATA CORRENTE
076600000830     C                   time                    wn14             14 0
076700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
076800000830     C                   z-add     wn8           g08dat
076900000830     C                   z-add     *zeros        g08inv
077000000830     C                   movel     '0'           g08err
077100000830     C                   call      'XSRDA8'
077200000830     C                   parm                    wlbda8
077300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
077400000830     C*
077500000613     C                   ENDSR
077600000613     C***
077700990908
077800071214     Otitv1d2p  E            riepilogo         2
077900990915     O                                              'Upload via Internet'
078000990915     O                                           +1 'Traduzione TIVIN00R -'
078100021113     O                                           +1 'FIVABWWR/FIVATWWR'
078200010201     O                                           +1 'Report testate bolle'
078300990915     O          E            riepilogo   2
078400990915     O                       wrkdata
078500990915     O                       wrkora              +1
078600990915     O                       procname            +1
078700990915     O          E            riepilogo   2
078800990915     O                                              'Cliente..................:'
078900990915     O                       VABCCM        z     +1
079000990915     O          E            riepilogo   2
079100990920     O                                              'Riferimento Strategi.....:'
079200990920     O                       vlrhdl              +1
079300990915     O          E            riepilogo   2
079400990915     O                                              'Giusti...................:'
079500010201     O                       §CTROKVB      2   +  1
079600990915     O          E            riepilogo   2
079700990915     O                                              'Sbagliati e corretti.....:'
079800971022     O                       §CTRMO        2   +  1
079900990915     O          E            riepilogo   2
080000990915     O                                              'Sbagliati e scartati.....:'
080100971022     O                       §CTRNO        2   +  1
080200000613
080300071214     Otitv1d2ps E            testdett          2
080400000613     O                                              'Upload via Internet'
080500000613     O                                           +1 'Traduzione TIVIN00R -'
080600021113     O                                           +1 'FIVABWWR/FIVATWWR'
080700010201     O                                           +1 'Report testate bolle'
080800000616     O          E            testdett    3
080900000613     O                                           +2 'N° rec'
081000000613     O                                           +3 'Anteprima contenuto'
081100000616     O          E            rigadett    2
081200000613     O                       rrnum               +2
081300000621     O                       recko               +3
081400000616     O          E            findett     2
081500000613     O                       wrkdata
081600000613     O                       wrkora              +1
081700000613     O                       procname            +1
081800000616     O          E            findett     2
081900000613     O                                              'Cliente..................:'
082000000613     O                       VABCCM        z     +1
082100000616     O          E            findett     2
082200000613     O                                              'Riferimento Strategi.....:'
082300000613     O                       vlrhdl              +1
082400000616     O          E            findett     2
082500000613     O                                              'Giusti...................:'
082600010201     O                       §CTROKVB      2   +  1
082700000616     O          E            findett     2
082800000613     O                                              'Sbagliati e corretti.....:'
082900000613     O                       §CTRMO        2   +  1
083000000616     O          E            findett     2
083100000613     O                                              'Sbagliati e scartati.....:'
083200000613     O                       §CTRNO        2   +  1
083300000512** CMD - COMANDI CL
083400071214OVRPRTF FILE(TITV1D2P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
083500071214OVRPRTF FILE(TITV1D2PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
083600071214DLTOVR FILE(TITV1D2P TITV1D2PS) LVL(*)
083700000512
083800000512
