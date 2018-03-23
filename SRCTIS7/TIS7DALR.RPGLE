000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700001018     Ftis7dalp  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900001018     Ftis7dalps O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600000724     D*------------
001700990920     D dscmz         e ds                  inz
001800990910     D psds           sds
001900990910     D  procname         *PROC
002000941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
002100941229     D* 10+3
002200941229     D                 DS
002300000313     D  WI15                   1     10
002400000313     D  WD15                  11     13
002500000313     D  WASC15                 1     13
002600941229     D*  6+1
002700941229     D                 DS
002800000313     D  WI09                   1      6
002900000313     D  WD09                   7      7
003000000313     D  WASC09                 1      7
003100941229     D*  2+3
003200941229     D                 DS
003300000313     D  WI07                   1      2
003400000313     D  WD07                   3      5
003500000313     D  WASC07                 1      5
003600000313     D*
003700990920     D tivlrds       e ds                  extname(tivlr00f)
003800000313     D*---------------------------------------------------------
003900000313     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
004000000313     D*---------------------------------------------------------
004100000313     D                 ds
004200010315     D rifnum                 51     57  0
004300010315     D rag                    81    115
004400010315     D ind                   116    150
004500010315     D loc                   151    185
004600010315     D cap                   186    195
004700010315     D naz                   196    199
004800010315     D prv                   203    205
004900010315     D fraass                233    234
005000010315     D col                   235    241  0
005100010315     D vol                   242    250  3
005200010315     D pes                   251    261  3
005300010315     D con                   285    301  5
005400010315     D div                   302    305
005500010315     D  X_rec                  1    345    inz
005600000313     D*
005700990910     D esito           s              1
005800000724     D prmlit          s             10
005900000710     D prmfir          s             10
006000990921     D wrkesito        s                   like(esito)
006100990915     D wrkdata         s               d
006200990915     D wrkora          s               t
006300000613     D rrnum           s              6  0 INZ(*zeros)
006400000621     D recko           s            150    INZ(*blanks)
006500011123     D depcmd          s            150    INZ(*blanks)
006600000313     D*
006700990915     C                   time                    wrkdata
006800990915     C                   time                    wrkora
006900000913     C                   reset                   rrnum
007000990921     C                   reset                   esito
007100990921     C                   reset                   wrkesito
007200000724     C*
007300000724     C* SE OCCORRE SPEDIRE IN FILIALE
007400000724     C                   if        vlrpoi <> *zeros
007500000724     C*
007600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007800000724     C     vlrpoi        chain     azorg01l
007900000724     C                   if        %found
008000000724     C                   movel(p)  CMD(1)        depcmd
008100020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008200000724     C*
008300000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008400011123     C                   Z-ADD     150           LENGH            15 5
008500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008600000724     C                   PARM                    depcmd
008700000724     C                   PARM                    LENGH
008800000724     C*
008900000724     C                   endif
009000020829     C                   endif
009100000616     C*
009200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009300000616     C                   movel(p)  CMD(2)        depcmd
009400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009500000616     C*
009600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009700011123     C                   Z-ADD     150           LENGH            15 5
009800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009900000616     C                   PARM                    depcmd
010000000616     C                   PARM                    LENGH
010100000613     C*
010200001018     C                   if        not %open(tis7dalps)
010300001018     C                   open      tis7dalps
010400000616     C                   except    testdett
010500000613     C                   endif
010600990915     C*
010700941229     C                   EXSR      RWVAB                                        LETT/SCR. VAB
010800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010900000613     C*
011000001018     C                   if        %open(tis7dalps)
011100000616     C                   except    findett
011200001018     C                   close     tis7dalps
011300000613     C                   endif
011400000616     C*
011500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011600011123     C                   Z-ADD     150           LENGH            15 5
011700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011800000616     C                   PARM                    CMD(3)
011900000616     C                   PARM                    LENGH
012000000616     C*
012100000613
012200000613     C                   return
012300000613
012400000613
012500000613     C*--------------------------------------------------------
012600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012700000613     C*--------------------------------------------------------
012800000621     C     STPR          BEGSR
012900000613     C*
013000001018     C                   if        not %open(tis7dalp)
013100001018     C                   open      tis7dalp
013200990915     C                   endif
013300990915     C*
013400990915     C                   except    riepilogo
013500990915     C*
013600001018     C                   if        %open(tis7dalp)
013700001018     C                   close     tis7dalp
013800990914     C                   endif
013900000613     C*
014000000613     C                   ENDSR
014100000613     C***
014200000613
014300910830     C*--------------------------------------------------------
014400021113     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
014500910830     C*--------------------------------------------------------
014600941229     C     RWVAB         BEGSR
014700990910     C*
014800990914     C                   if        not %open(tivin00r)
014900990908     C                   open      tivin00r
015000990914     C                   endif
015100021113     C                   if        not %open(fivabwwr)
015200021113     C                   open      fivabwwr
015300990914     C                   endif
015400990910     C*
015500990910     C                   clear                   §CTROK
015600990910     C                   clear                   §CTRMO
015700990910     C                   clear                   §CTRNO
015800990910     C*
015900921023     C                   DO        *HIVAL
016000990913     C*
016100990915     C                   READ      tivin00r                               70
016200000905     C                   if        vindta > *blanks
016300000613     C                   add       1             rrnum
016400001018     C*
016500990910     C                   if        *in70 = *off
016600990913     C                             and
016700990913     C                             (vinflg = *blanks
016800990913     C                              or vinflg = '0'
016900990913     C                              or vinflg = '2')
017000990913     C*
017100001018     C                   exsr      defcam
017200001018     C*
017300990913     C                   clear                   vinmsg
017400000313     C                   eval      x_rec  = vindta
017500001018     C*
017600001018     C                   movel     '1 '          VABCBO
017700001018     C     fraass        ifne      '02'
017800001018     C                   movel     '2 '          VABCBO                         ASS.TO
017900001018     C                   endif
018000001018     C                   z-add     con           VABCAS
018100001018     C     VABCAS        ifgt      *zeros
018200001018     C     VABCBO        ifeq      '1 '
018300001018     C                   movel     '4 '          VABCBO                         FRANCO+C/A
018400001018     C                   else
018500001018     C                   movel     '6 '          VABCBO                         ASS.TO+C/A
018600001018     C                   endif
018700001018     C                   endif
018800001018     C                   z-add     col           VABNCL
018900001018     C                   z-add     pes           VABPKB
019000001018     C                   z-add     vol           VABVLB
019100001018     C                   z-add     rifnum        VABRMN
019200010315     C                   movel     rag           VABRSD
019300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
019400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
019500020117     C* ==
019600010315     C                   movel     ind           VABIND
019700010315     C                   movel     cap           VABCAD
019800010315     C                   movel     loc           VABLOD
019900010315     C                   movel     prv           VABPRD
020000010315     C                   if        %trim(naz) <> 'I'
020100010315     C                   movel     naz           VABNZD
020200010315     C                   endif
020300010315     C                   if        %trim(div) = 'LIT'
020400010315     C                   movel     'ITL'         VABVCA
020500010315     C                   endif
020600930409     C*
020700001018     C  N31              ADD       1             §CTROK            5 0
020800971021     C   32              ADD       1             §CTRMO            5 0
020900971021     C   31              ADD       1             §CTRNO            5 0
021000021113     C  N31              WRITE     FIVAB000
021100990910     C*
021200990910     C                   if        *in31 = *off
021300990910     C                   eval      vinflg = '1'
021400990910     C                   else
021500000621     C                   eval      recko = vindta
021600000616     C                   except    rigadett
021700990910     C                   eval      vinflg = '2'
021800990910     C                   endif
021900000905     C*
022000000905     C                   endif
022100000905     C*
022200000905     C                   else
022300000905     C                   eval      vinflg = '1'
022400000905     C                   endif
022500000905     C*
022600000905     C  N70              update    tivin000
022700000905     C*
022800991022     C  N70              ENDdo
022900990910
023000990910     C* Se non ci sono record con errori ...
023100990910     C                   if        §ctrno = 0
023200990910     C* ... restituisco esito OK.
023300990921     C                   eval      wrkesito = '0'
023400990910     C                   else
023500000710     C                   if        §ctrok > 0
023600000710     C                   eval      wrkesito = '1'
023700000710     C                   else
023800000710     C                   eval      wrkesito = '2'
023900990910     C                   endif
024000000710     C                   endif
024100990910     C*
024200990914     C                   if        %open(tivin00r)
024300990908     C                   close     tivin00r
024400990914     C                   endif
024500021113     C                   if        %open(fivabwwr)
024600021113     C                   close     fivabwwr
024700990914     C                   endif
024800990910     C*
024900990920     C                   if        §ctrok > 0
025000000724     C                             and vlrpoi <> *zeros
025100990920     C                   exsr      invio
025200990920     C                   endif
025300990920     C*
025400910830     C                   ENDSR
025500000613     C***
025600001018
025700001018
025800001018     C*--------------------------------------------------------
025900001018     C* DEFCAM  -  IMPOSTAZIONE CAMPI A VALORE FISSO          *
026000001018     C*--------------------------------------------------------
026100001018     C     DEFCAM        BEGSR
026200001018     C*
026300021113     C                   clear                   fivab000
026400001018     C                   setoff                                       3132
026500001018     C*
026600001018     C                   z-add     053           VABLNP
026700001018     C                   z-add     0530002       VABCCM
026800001018     C                   z-add     000           VABCTR
026900001018     C                   time                    DATA14           14 0
027000001018     C                   move      DATA14        VABAAS
027001021113     C                   move(p)   vlrpoi        VABFGS
027100001018     C*
027200001018     C                   ENDSR
027300001018     C***
027400001018
027500001018
027600990920
027700990920      /TITLE Invio dei dati al punto operativo.
027800000613     C     invio         BEGSR
027900990920     C*
028000990920     C                   reset                   dscmz
028100990920     C                   move      vlrpoi        cmzdst
028200990920     C                   eval      cmzfld = vlrfou
028300990920     C                   eval      cmzmbd = vlrhdl
028400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
028500000710     C                   if        prmfir = *blanks
028600021113     C                   eval      cmzfla = 'FIVAB00F'
028700021113     C                   eval      cmzmba = 'FIVAB00F'
028800000710     C                   else
028900000710     C                   eval      cmzfla = prmfir
029000000710     C                   eval      cmzmba = prmfir
029100000710     C                   endif
029200990920     C                   eval      cmznrr = *zeros
029300990920     C                   move      §ctrok        cmznrr
029400021025     C                   eval      cmzlba = vlrfl1
029500990920     C                   call(e)   'TIS711C'
029600990920     C                   parm                    dscmz
029700990921     C                   parm      *blanks       esito
029800990920     C                   if        %error
029900990920     C                             or cmzerr = '1'
030000990921     C                             or esito  = '1'
030100000710     C                   eval      wrkesito = '3'
030200990920     C                   endif
030300990920     C*
030400000613     C                   ENDSR
030500990910
030600001018
030700001018
030800000613     C     *inzsr        BEGSR
030900990910     C*
031000990910     C     *entry        plist
031100990920     C                   parm                    tivlrds
031200990921     C                   parm      wrkesito      esito
031300000724     C                   parm                    prmlit
031400000710     C                   parm                    prmfir
031500000613     C*
031600000613     C                   ENDSR
031700000613     C***
031800990908
031900001018
032000001018     Otis7dalp  E            riepilogo         2
032100990915     O                                              'Upload via Internet'
032200990915     O                                           +1 'Traduzione TIVIN00R -'
032300021113     O                                           +1 'FIVABWWR'
032400990915     O                                           +1 'Testate bolle'
032500990915     O          E            riepilogo   2
032600990915     O                       wrkdata
032700990915     O                       wrkora              +1
032800990915     O                       procname            +1
032900990915     O          E            riepilogo   2
033000990915     O                                              'Cliente..................:'
033100990915     O                       VABCCM        z     +1
033200990915     O          E            riepilogo   2
033300990920     O                                              'Riferimento Strategi.....:'
033400990920     O                       vlrhdl              +1
033500990915     O          E            riepilogo   2
033600990915     O                                              'Giusti...................:'
033700971022     O                       §CTROK        2   +  1
033800990915     O          E            riepilogo   2
033900990915     O                                              'Sbagliati e corretti.....:'
034000971022     O                       §CTRMO        2   +  1
034100990915     O          E            riepilogo   2
034200990915     O                                              'Sbagliati e scartati.....:'
034300971022     O                       §CTRNO        2   +  1
034400000613
034500001018     Otis7dalps E            testdett          2
034600000613     O                                              'Upload via Internet'
034700000613     O                                           +1 'Traduzione TIVIN00R -'
034800021113     O                                           +1 'FIVABWWR'
034900000613     O                                           +1 'Testate bolle'
035000000616     O          E            testdett    3
035100000613     O                                           +2 'N° rec'
035200000613     O                                           +3 'Anteprima contenuto'
035300000616     O          E            rigadett    2
035400000613     O                       rrnum               +2
035500000621     O                       recko               +3
035600000616     O          E            findett     2
035700000613     O                       wrkdata
035800000613     O                       wrkora              +1
035900000613     O                       procname            +1
036000000616     O          E            findett     2
036100000613     O                                              'Cliente..................:'
036200000613     O                       VABCCM        z     +1
036300000616     O          E            findett     2
036400000613     O                                              'Riferimento Strategi.....:'
036500000613     O                       vlrhdl              +1
036600000616     O          E            findett     2
036700000613     O                                              'Giusti...................:'
036800000613     O                       §CTROK        2   +  1
036900000616     O          E            findett     2
037000000613     O                                              'Sbagliati e corretti.....:'
037100000613     O                       §CTRMO        2   +  1
037200000616     O          E            findett     2
037300000613     O                                              'Sbagliati e scartati.....:'
037400000613     O                       §CTRNO        2   +  1
037500000613** CMD - COMANDI CL
037600011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7DALP)  FORMTYPE(RICCLI) OUTQ(
037700011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7DALPS) FORMTYPE(RICCLI) OUTQ(
037800001018DLTOVR FILE(TIS7DALP TIS7DALPS) LVL(*)
037900000613
038000000613
