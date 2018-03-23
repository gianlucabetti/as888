000100030930      *--------------------------------------------------------------------
000200031001     D SndMg01         S             10                                         Message type
000300031001     D                                     INZ('*INFO')
000400031001     D SndMg02         S             10                                         Deluvery mode
000500031001     D                                     INZ('*BREAK')
000600031001     D SndMg03         S            256                                         Message text
000700031001     D SndMg04         S             10I 0                                      Length of text
000800031001     D                                     INZ(%SIZE(SndMg03))
000900031001     D SndMg05         S             10                                         User profile
001000031001     D                                     DIM(299)
001100031001     D SndMg06         S             10I 0                                      Number of user
001200031001     D SndMg07         S             10I 0                                      Message sent indic.
001300031001     D SndMg08         S             10I 0                                      Function requested
001400031001     D SndMg10         S              1                                         Show display
001500031001     D                                     INZ('N')
001600031001     D SndMg11         S             20                                         Qualified MSGQ name
001700031001     D SndMg12         S              4                                         Name type indicator
001800031001     D                                     INZ('*USR')
001900031001      * indice di scihera
002000031001     D X               S              5I 0
002100031001      *
002200031001     D/COPY QSYSINC/QRPGLESRC,QUSEC
002300031001      *
002400030930      *---------------------------------------------------------------------*
002500031001      * INVIA MESSAGGIO
002600031001      *---------------------------------------------------------------------*
002700031001     c                   z-add     1             x
002800031001     C                   exsr      CalQEZSNDMG
002900030926      *
003000000000     C                   SETON                                        LR
003100031001     ***********************************************************************
003200031001     **
003300031001     ** Send Message (QEZSNDMG) API
003400031001     **
003500031001     ***********************************************************************
003600031001     C     CalQEZSNDMG   BEGSR
003700031001      *
003800031001     ** Invio messaggio all'utente.
003900031001     C                   EVAL      SndMg03 = 'ATTENZIONE:  Procedura saldi FICN-
004000031001     C                             32R ha riscontrato un disallineamento fra FI-
004100031001     C                             FTD00F e FIFCE00F ricevuti. Non sono arrivat-
004200031001     C                             i FIFTD da filiale vedere stampa di controll-
004300031001     C                             o: << WRKSPLF SELECT(*ALL *ALL *ALL FICN32R)-
004400031001     C                              >> in Sede.'
004500031001     C                   EVAL      SndMg05(x) = 'EDPAB     '
004600031001     C                   EVAL      SndMg06 = x
004700031001     C                   CLEAR                   QUSEC
004800031001     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
004900031001
005000031001     C                   CALL      'QEZSNDMG'
005100031001     C                   PARM                    SndMg01
005200031001     C                   PARM                    SndMg02
005300031001     C                   PARM                    SndMg03
005400031001     C                   PARM                    SndMg04
005500031001     C                   PARM                    SndMg05
005600031001     C                   PARM                    SndMg06
005700031001     C                   PARM                    SndMg07
005800031001     C                   PARM                    SndMg08
005900031001     C                   PARM                    Qusec
006000031001     C                   PARM                    SndMg10
006100031001     C                   PARM                    SndMg11
006200031001     C                   PARM                    SndMg12
006300031001      *
006400031001     C                   ENDSR
006500031001      **********************************************************************
