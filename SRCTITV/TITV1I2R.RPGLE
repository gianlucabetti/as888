000100080801      /TITLE Upload via Internet: traduzione Riferimenti file in IFS
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000700010330     D*----------------------------------------------------
000800070124     D* DICHIARAZIONE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000010330     D psds           sds
001100010330     D  procname         *PROC
001200010330     D tivlrds       e ds                  extname(tivlr00f)
001300010330     D esito           s              1
001400080314     D prmlit          s             10
001500080314     D prmfir          s             10
001600010330     D wrkesito        s                   like(esito)
001700010330     D wrkdata         s               d
001800010330     D wrkora          s               t
001900080314     D rrnum           s              6  0 INZ(*zeros)
002000010330     D*------------------
004600080109     C*
004700990915     C                   time                    wrkdata
004800990915     C                   time                    wrkora
004900000913     C                   reset                   rrnum
005000990921     C                   reset                   esito
005100990921     C                   reset                   wrkesito
005200010601     C*
005300010601     C                   exsr      opeini
005400060803     C                   exsr      rwfile
005500010601     C*
005600060905     C                   seton                                        LR
005700010601     C***
005800010330     C*--------------------------------------------------------
005900060803     C* RWFILE  LEGGE TIVIN00R E SCRIVE FIARG                 *
006000010330     C*--------------------------------------------------------
006100060803     C     RWFILE        BEGSR
006200010330     C*
006300010330     C                   if        not %open(tivin00r)
006400010330     C                   open      tivin00r
006500010330     C                   endif
006600010330     C*
006700010604     C                   clear                   低TROK
006800010604     C                   clear                   低TRNO
006900060720     C*
007000060720     C                   exsr      defcam
007100010330     C*
007200080109    1C                   DO        *HIVAL
007300010330     C*
007400010330     C                   READ      tivin00r                               70
007500010618     C*
007600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
007700080109    2C                   if        vinflg = '1'
007800010618     C                   eval      flgOk = '1'
007900080109    2C                   endif
008000010618     C*
008100080109    2C                   if        vindta > *blanks
008200010330     C                   add       1             rrnum
008300010330     C*
008400080109    3C                   if        *in70 = *off and
008500010330     C                             (vinflg = *blanks
008600010330     C                              or vinflg = '0'
008700010330     C                              or vinflg = '2')
008800010330     C*
008900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
009000080109    4C                   if        vinflg = *blanks or vinflg = '0'
009100010711     C                   clear                   vinmsg
009200080109    4C                   endif
009300010601     C*
009400060803     C* Esegue l'effettiva acquisizione dei dati
009500060803     C                   exsr      impfile
009600010604     C*
009700080109    3C                   endif
009800020722     C*
009900080314    3C                   if        *in31 = *off and
010000010604     C                             *in32 = *off
010100010604     C                   eval      vinflg = '1'
010200010604     C                   else
010300010604     C                   eval      vinflg = '2'
010400080109    3C                   endif
010500010604     C*
010600010330     C                   else
010700010330     C                   eval      vinflg = '1'
010800080109    2C                   endif
010900010601     C*
011000010601     C  N70              update    tivin000
011100010330     C*
011200080109    1C  N70              ENDdo
011300080314     C*
011400080314     C* Ebbene...
011500080314     C  N31              ADD       1             低TROK            7 0
011600080314     C   31              ADD       1             低TRNO            7 0
011700080314     C*
011800080801     C  N31              EXSR      LANPGM
011900010601     C*
012000010330     C* Se non ci sono record con errori ...
012100080314     C                   if        呃trno = 0
012200010330     C* ... restituisco esito OK.
012300010330     C                   eval      wrkesito = '0'
012400010330     C                   else
012500010330     C                   if        呃trok > 0
012600010330     C                   eval      wrkesito = '1'
012700010330     C                   else
012800010615     C                   if        flgOk = '0'
012900010615     C                   eval      wrkesito = '2'
013000010615     C                   else
013100010615     C                   eval      wrkesito = '6'
013200010615     C                   endif
013300010330     C                   endif
013400010330     C                   endif
013500010330     C*
013600010330     C                   if        %open(tivin00r)
013700010330     C                   close     tivin00r
013800010330     C                   endif
013900010330     C*
014000010330     C                   ENDSR
014100010330     C***
014200010601
014300010601
014400010330     C*----------------------------------------------------*
014500020722     C*  IMPOSTAZIONE CAMPI COSTANTI
014600010330     C*----------------------------------------------------*
014700010330     C     DEFCAM        BEGSR
014800080314     C*
014900080314     C                   SETOFF                                       3132
014901080801     C*
014902080801     C* Inizializzo variabili d wrk
014904080801     C                   Z-ADD     *ZEROS        wGiro             3 0
014905080801     C                   MOVEL     *BLANKS       wLIB             10
014906080801     C                   MOVEL     *BLANKS       wPGM             10
014907080801     C                   MOVEL     *BLANKS       wOPZ              2
014908080801     C                   MOVEL     *BLANKS       wPPT             30
014909080801     C                   MOVEL     *BLANKS       wPATHIFS        255
014910080801     C                   MOVEL     *BLANKS       wFILEIFS        255
014911080801     C                   MOVEL     *BLANKS       wFILEORI        255
014912080801     C                   MOVEL     *BLANKS       wESITO            1
015300080314     C*
015400020204     C* Imposto i valori di default...
015500020204     C* ... e poi verifico se sono stati passati come parametri
015600020204     C                   IF        vlrppt > *blanks
015601080801     C                   EVAL      wLIB = %subst(vlrppt:1:10)
015602080801     C                   EVAL      wPGM = %subst(vlrppt:11:10)
015603080801     C                   EVAL      wOPZ = %subst(vlrppt:21:2)
015604080801     C                   EVAL      wPPT = %subst(vlrppt:23:20)
015700020204     C                   ENDIF
015800020204     C*
015900010330     C                   ENDSR
015901080801
015902080801
016000010607     C*----------------------------------------------------*
016100080314     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X TIESV)
016200010607     C*----------------------------------------------------*
016300070112     C     IMPFILE       BEGSR
024900020204     C*
024901080801     C                   ADD       1             wGiro
024902080801     C*
024903080801     C                   IF        wGiro = 1
024904080801     C                   EVAL      wPATHIFS = %trim(vindta)
024905080801     C                   ENDIF
024906080801     C*
024907080801     C                   IF        wGiro = 2
024908080801     C                   EVAL      wFILEIFS = %trim(vindta)
024909080801     C                   ENDIF
024910080801     C*
024911080801     C                   IF        wGiro = 3
024912080801     C                   EVAL      wFILEORI = %trim(vindta)
024913080801     C                   ENDIF
024914080801     C*
025000080314     C                   ENDSR
034100010330
034102080801
034103080801     C*----------------------------------------------------*
034104080801     C*  CHIAMATA AL PGM SPECIFICO
034105080801     C*----------------------------------------------------*
034106080801     C     LANPGM        BEGSR
034107080801     C*
034109080801     C                   CALL(e)   'TITV1I2C'
034110080801     C                   PARM                    wOPZ
034111080801     C                   PARM                    wLIB
034112080801     C                   PARM                    wPGM
034113080801     C                   PARM                    wPPT
034114080801     C                   PARM                    wPATHIFS
034115080801     C                   PARM                    wFILEIFS
034116080801     C                   PARM                    wFILEORI
034117080801     C                   PARM                    wESITO
034118080801     C*
034119080801     C                   IF        %error OR
034120080801     C                             wESITO <> '0'
034121080801     C                   EVAL      呃trno = 1
034122080801     C                   ENDIF
034123080801     C*
034124080801     C                   ENDSR
034125080801
034126080801
036300010601
036400060906     C*----------------------------------------------------*
036500060803      /TITLE Operazioni iniziali
036600060906     C*----------------------------------------------------*
036700010601     C     opeini        BEGSR
036800010601     C*
036900010601     C* Inizializzo flag e contatori operativi
037000010601     C                   movel     '0'           flgGiro           1
037100010615     C                   movel     '0'           flgOk             1
037200010601     C*
037300010601     C                   ENDSR
037400010601     C***
037500010330
037501080801
037600060906     C*----------------------------------------------------*
037700000613     C     *inzsr        BEGSR
037800060906     C*----------------------------------------------------*
037900990910     C*
038000990910     C     *entry        plist
038100990920     C                   parm                    tivlrds
038200990921     C                   parm      wrkesito      esito
038300000724     C                   parm                    prmlit
038400000710     C                   parm                    prmfir
039500000613     C*
039600000613     C                   ENDSR
