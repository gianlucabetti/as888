000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000201120315     D* DS PER TRUL90R - RICHIESTA STAMPANTI
000202120315     D Trul90DS      E DS                  inz
000203120315     D KPJBA         E DS
000300120315     D PARAM           DS                  INZ
000800120315     D*
000900120315     D  V1CLPD                23     25  0
001000120315     D  V1CSED                26     27  0
001100120315     D  V1CNSD                28     34  0
001200120315     D*
001500120315     D  V1CAAS                44     47  0
002800120315     D*
002900120315     D  VSCSNP                86     86
003200120315
003300120315      * PARFLG = 'E' --> STAMPATE PIU' DI 6000 PAGINE  impostare a blanks
003400120315     D  PARFLG               156    156
003500120315     d* - parametri da TRUL90R:
003600120315     d* · pgm.   per stampa LDV
003700120315     d  wD90psl              157    166
003800120315     d* · stampante per LDV in formato "A4" (laser)
003900120315     d  wD90prb4             167    176
004000120315     d* · stampante per LDV in formato "A5" (laser)
004100120315     d  wD90prb5             177    186
004200120315     d* · modulo per stampa LDV su "A4"     (laser)
004300120315     d  wD90mdb4             187    196
004400120315     d* · modulo per stampa LDV su "A5"     (laser)
004500120315     d  wD90mdb5             197    206
004600120315     d* · pgm di stampa etichette segnacolli
004700120315     d  wD90pss              207    216
004800120315     d* · modulo per stampa etichette
004900120315     d  wD90mde              227    236
005000120315     d* · stampante per etichette
005100120315     d  wD90pre              247    256
005200120315      *
005201120316     c     *entry        plist
005202120316     c                   parm                    kpjba
005203120316     c                   movel     kpjbu         param
005204120316
005205120316     c*
005300120315     C                   MOVEL     'S'           d90RSE
005400120315     C                   MOVEL     'S'           d90RSB
005500120315     C                   CALL      'TRUL90R'
005600120315     C                   PARM                    KPJBA
005700120315     C                   PARM                    trul90Ds
005701120316     c*
005800120315     C*
005900120315      * Parametri x ovrprtf
006000120315      * · stampante per etichette
006100120315     c                   movel     D90pre        wD90pre
006200120315      * · modulo per stampa etichette
006300120315     c                   movel     D90mde        wD90mde
006400120315      * · pgm. di stampa etichette segnacolli
006500120315     c                   movel     D90pss        wD90pss
006600120315      * · stampante per LDV in formato "A4" - laser
006700120315     c                   movel     D90prb4       wD90prb4
006800120315      * · stampante per LDV in formato "A5" - laser
006900120315     c                   movel     D90prb5       wD90prb5
007000120315      * · modulo per stampa LDV su "A4" ... - laser
007100120315     c                   movel     D90mdb4       wD90mdb4
007200120315      * · modulo per stampa LDV su "A5" ... - laser
007300120315     c                   movel     D90mdb5       wD90mdb5
007400120315      * · pgm.   per stampa LDV ........... - ad aghi o laser
007500120315     c                   movel     D90psl        wD90psl
007600120315     C*
007700120315     C**
007800120315     C* F3 - FINE
007900120315     C     d90F3         IFEQ      '1'
008000120315     C                   GOTO      FINE
008100120315     C                   ENDIF
008101120316     c                   movel     param         kpjbu
008102120316     c*
008103120315     C                   CALL      'FNLS76C'
008104120315     C                   PARM                    KPJBA
008200120315     C     FINE          TAG
008300120315     C                   SETON                                        LR
