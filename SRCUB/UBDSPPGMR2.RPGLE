000100140819     FWFACTGR0F O    E             DISK
000200140819     D pIn_RcvVar      s            500a
000300140819     D pIn_ErrCode     s              8a
000400140819     D Time            s               z
000500140819     D PGM0100         ds           500
000600140819     D  Pgm                    9     18a
000700140819     D  Lib                   19     28a
000800140819     D  Attr                  39     48a
000900140819     D  UsrPrf               106    106a
001000140819     D  Descr                111    160a
001100140819     D  ActGrp               369    398a
001200140819     C     *ENTRY        PLIST
001300140819     C                   PARM                    pIn_RcvVar
001400140819     C                   PARM                    pIn_ErrCode
001500140819     C*
001600140819     C                   IF        Time = *loval
001700140819     C                   EVAL      Time = %timestamp()
001800140819     C                   ENDIF
001900140819     C*
002000140819     C* valorizzo i campi del rcd
002100140819     C                   EVAL      PGM0100 = pIn_RcvVar
002200140819     C                   EVAL      ACTTime = Time
002300140819     C                   EVAL      ACTActGrp = ActGrp
002400140819     C                   EVAL      ACTPgm = Pgm
002500140819     C                   EVAL      ACTLib = Lib
002600140819     C                   EVAL      ACTAttr = Attr
002700140819     C* testo se c'è stato unm errore
002800140819     C                   IF        %subst(pIn_ErrCode : 5 : 4) <> *blank
002900140819     C                   EVAL      ACTDescr = 'ERRORE NEL REPERIMENTO DATI'
003000140819     C                   ELSE
003100140819     C                   EVAL      ACTDescr = Descr
003200140819     C                   ENDIF
003300140819     C                   SELECT
003400140819     C                   WHEN      UsrPrf = 'O'
003500140819     C                   EVAL      ACTUsrPrf = '*OWNER'
003600140819     C                   OTHER
003700140819     C                   EVAL      ACTUsrPrf = '*USER'
003800140819     C                   ENDSL
003900140819
004000140819     C                   WRITE     WFACTGR0
004100140819
004200140819     C                   SETON                                        RT
