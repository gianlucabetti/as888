000100030401     H*--------------------------------------------------------------------------------------------*
000200030401     H DECEDIT('0,') DATEDIT(*DMY.)
000300150618     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000400030401     H*--------------------------------------------------------------------------------------------*
000500160112     FTNTBE01L  IF   E           K DISK
000600160127     FWFMAF050F O    E             DISK
000700030404     D*---
000800030404     D* DS per chiamata pgm utilità x passaggio da spool a file *FCFC "normalizzato"
000900030404     D*---
001000091211     D TRBMFCDS      e ds
001100160112     D*---
001200160112     D* DS ridefinizione tabelle di procedura
001300160112     D*---
001400160112     D DAOA          e ds
001500160115     D*---
001600160115     D* Variabili di wrk
001700160115     D*---
001800160203     D SUBFLDR_ARCHI   s                   like(SUBFLDR) inz
001900160203     D KTBECOD         s                   like(TBECOD) inz
002000160203     D KTBEKE1         s                   like(TBEKE1) inz
002100160203     D KTBEKE2         s                   like(TBEKE2) inz
002200171011     D wFLGREPLACE     s                   like(FLGREPLACE) inz
002300160112     D*---
002400160127     D* Program Status DS
002500160127     D*---
002600160127     D psds           sds
002700160127     D  jobname              244    253
002800160127     D  jobuser              254    263
002900160127     D  jobnumber            264    269
003000160127
003100160127
003200070607     C*
003300070607     C* Ricevimento parametri
003400090513     C     *ENTRY        PLIST
003500160129     C                   PARM                    APLKEY           15
003600090514     C                   PARM                    IFSNAM           10
003700090513     C                   PARM                    FILNAM           10
003800090513     C                   PARM                    JOBNAM           10
003900090513     C                   PARM                    USRNAM           10
004000090513     C                   PARM                    JOBNUM            6
004100090513     C                   PARM                    NUMFIL            6
004200160112     C                   PARM                    FLGDATAORA        1
004300160112     C                   PARM                    FILEOUT         255
004400090514     C                   PARM                    SUBFLDR          50
004500160115     C                   PARM                    FLGSUBFLDR        1
004600160120     C                   PARM                    FLGREPLACE        1
004700160127     C                   PARM                    FLGLOG            1
004800160119     C                   PARM                    ESITO             1
004900160112     C*
005000160115     C* Reperisco la DATA e ORA corrente
005100160112     C                   TIME                    wn14             14 0
005200160112     C                   MOVEL     wn14          ORACOR            6 0          *ORA
005300160112     C                   Z-ADD     *zeros        DATCOR            8 0          *DATA CORRENTE AA/M/
005400160112     C                   EVAL      DATCOR = %dec(%date() : *ISO)
005500160129     C*
005600160115     C* Gestisco se richiesto automatismo per aggiunta automatica timestamp al nome del file
005700160119     C                   MOVEL     *blanks       wDATAORA         15
005800160115     C                   IF        FLGDATAORA = 'S'
005900160112     C                   EVAL      wDATAORA = '_' + %trim(%editc(DATCOR:'X')) +
006000160112     C                                              %trim(%editc(ORACOR:'X'))
006100160112     C                   ENDIF
006200160112     C*
006300160112     C* Definizione chiavi di lettura
006400160112     C*
006500160112     C* TNTBE01L - Parziale
006600160112     C     KEYTBE01_P    KLIST
006700160203     C                   KFLD                    KTBECOD
006800160203     C                   KFLD                    KTBEKE1
006900160203     C                   KFLD                    KTBEKE2
007000160112     C*
007100160112     C* Reperisco la descrizione della fase da tabella
007200160203     C                   MOVEL(p)  'AOA'         KTBECOD
007300160203     C                   MOVEL(p)  APLKEY        KTBEKE1
007400160203     C                   MOVEL(p)  DATCOR        KTBEKE2
007500160112     C                   CLEAR                   DAOA
007600160203     C                   SETOFF                                       33
007700160203     C     KEYTBE01_P    SETGT     tntbe01l
007800160112     C                   IF        %found(tntbe01l)
007900160203     C                   DOW       not %eof(tntbe01l) AND
008000160203     C                             *IN33 = *OFF
008100160203     C                   READP     tntbe01l
008200160203     C                   IF        TBEKE1  = KTBEKE1
008300160203     C                   IF        TBEKE2 <= KTBEKE2 AND
008400160203     C                             TBEATB  = *blanks
008500160203     C                   SETON                                        33
008600160203     C                   ENDIF
008700160203     C                   ELSE
008800160203     C                   LEAVE
008900160203     C                   ENDIF
009000160203     C                   ENDDO
009100160203     C                   ENDIF
009200160203     C*
009300160203     C                   IF        *IN33
009400160129     C                   MOVEL(P)  TBEUNI        DAOA
009500090514     C*
009600090514     C* Verifico correttezza formale sotto-cartella
009700090514     C                   IF        SUBFLDR <> *blanks AND
009800090514     C                             %subst(%trim(SUBFLDR):%len(%trim(SUBFLDR)):1)
009900090514     C                             <> '/'
010000090514     C                   EVAL      SUBFLDR = %trim(SUBFLDR) + '/'
010100090514     C                   ENDIF
010200160203     C*
010300160203     C* Effettuo mappatura tra struttura folder DocFlow e ArchiDoc
010400160203     C                   SELECT
010500160203     C                   WHEN      SUBFLDR = 'FATD/'
010600160203     C                   EVAL      SUBFLDR_ARCHI = 'FATTURE/DISTINTE/'
010700160203     C                   WHEN      SUBFLDR = 'FATA/'
010800160203     C                   EVAL      SUBFLDR_ARCHI = 'FATTURE/FATSEDE/'
010900160203     C                   WHEN      SUBFLDR = 'FAT9/'
011000160203     C                   EVAL      SUBFLDR_ARCHI = 'FATTURE/FATFILIALE/'
011100160203     C                   WHEN      SUBFLDR = 'FATE/'
011200160203     C                   EVAL      SUBFLDR_ARCHI = 'FATTURE/'
011300160219     C                   WHEN      SUBFLDR = 'FTMB/'
011400160219     C                   EVAL      SUBFLDR_ARCHI = 'FATMAN/'
011500160219     C                   WHEN      SUBFLDR = 'FTMX/'
011600160219     C                   EVAL      SUBFLDR_ARCHI = 'FATMAN/'
011700160606     C                   WHEN      SUBFLDR = 'ABON/'
011800160606     C                   EVAL      SUBFLDR_ARCHI = 'MODBON/'
011900160606     C                   WHEN      SUBFLDR = 'LGIA/'
012000160606     C                   EVAL      SUBFLDR_ARCHI = 'GIACENZE/'
012100160606     C                   WHEN      SUBFLDR = 'LGIC/'
012200160606     C                   EVAL      SUBFLDR_ARCHI = 'GIACENZE/'
012300160606     C                   WHEN      SUBFLDR = 'ABAR/'
012400160606     C                   EVAL      SUBFLDR_ARCHI = 'AVVDAN/'
012500160606     C                   WHEN      SUBFLDR = 'ANLT/'
012600160606     C                   EVAL      SUBFLDR_ARCHI = 'AVVDAN/'
012700160606     C                   WHEN      SUBFLDR = 'ADOC/'
012800160606     C                   EVAL      SUBFLDR_ARCHI = 'AVVDAN/'
012900160205     C                   OTHER
013000160205     C                   EVAL      SUBFLDR_ARCHI = SUBFLDR
013100160203     C                   ENDSL
013200001006     C*
013300091211     C* Valorizzo la DS pgm utilità TRBMFCC
013400091211     C                   CLEAR                   TRBMFCDS
013500030404     C                   EVAL      DFCFNAM  = FILNAM
013600030404     C                   EVAL      DFCJNAM  = JOBNAM
013700030404     C                   EVAL      DFCUNAM  = USRNAM
013800030404     C                   EVAL      DFCJNUM  = JOBNUM
013900030404     C                   EVAL      DFCFNUM  = NUMFIL
014000091211     C                   EVAL      DFCFOUT  = 'TRBMUWWF'
014100040907     C                   EVAL      DFCLOUT  = 'QTEMP'
014200030514     C                   EVAL      DFCFOPT  = 'R'
014300150618     C*
014400150618     C* Verifico composizione del codice opzione secondo la seguente sintassi:
014500160112     C*
014600150618     C* - 1° byte => pilota popolamento ambiente Archidoc
014700150618     C*              valori possibili 'N' = NO
014800150618     C*                      diversamente = SI
014900160112     C*
015000150618     C* - 2° byte => pilota popolamento ambiente DocFlow
015100150618     C*              valori possibili 'T' = TEST
015200150618     C*                               'P' = PROD
015300150618     C*                      diversamente = NESSUNO
015400160112     C*
015500150618     C* - 3° byte => pilota popolamento ambiente DocFlow Storico
015600150618     C*              valori possibili 'S' = SI
015700150618     C*                      diversamente = NO
015800160112     C*
015900160112     C                   MOVEL     *blanks       ROOT            100
016000160119     C                   MOVEL     *blanks       ESITO
016100150618     C*
016200150618     C* Eseguo le chiamate richieste secondo codice opzione ricevuto in input
016300160129     C                   IF        §AOAARCHI <> 'N'
016400160115     C                   EVAL      ROOT = '/Ottico/'
016500150618     C                   CALL(e)   'TRBMAF05C'
016600090514     C                   PARM                    IFSNAM
016700030401     C                   PARM                    FILNAM
016800030401     C                   PARM                    JOBNAM
016900030401     C                   PARM                    USRNAM
017000030401     C                   PARM                    JOBNUM
017100030401     C                   PARM                    NUMFIL
017200150618     C                   PARM                    ROOT
017300160203     C                   PARM                    SUBFLDR_ARCHI
017400091211     C                   PARM                    TRBMFCDS
017500160112     C                   PARM                    FILEOUT
017600160112     C                   PARM      *blanks       DATAORA          15
017700160115     C                   PARM                    FLGSUBFLDR
017800171011     C                   PARM                    FLGREPLACE
017900160119     C                   PARM                    ESITO
018000150618     C                   ENDIF
018100001006     C*
018200160129     C                   IF        §AOADOCFLW = 'T'
018300160513     C***                EVAL      ROOT = '/QNTC/cednetappvf14/Docflow/' +
018400160513     C***                                 'TEST/'
018500160513     C                   EVAL      ROOT = '/QNTC/cedfsbo2/Docflow/' +
018600160513     C                                    'TEST/'
018700150618     C                   CALL(e)   'TRBMAF05C'
018800150618     C                   PARM                    IFSNAM
018900150618     C                   PARM                    FILNAM
019000150618     C                   PARM                    JOBNAM
019100150618     C                   PARM                    USRNAM
019200150618     C                   PARM                    JOBNUM
019300150618     C                   PARM                    NUMFIL
019400150618     C                   PARM                    ROOT
019500150618     C                   PARM                    SUBFLDR
019600150618     C                   PARM                    TRBMFCDS
019700160112     C                   PARM                    FILEOUT
019800160112     C                   PARM      *blanks       DATAORA          15
019900160115     C                   PARM                    FLGSUBFLDR
020000160120     C                   PARM                    FLGREPLACE
020100160119     C                   PARM                    ESITO
020200150618     C                   ENDIF
020300160120     C*
020400160120     C* - Per "sicurezza" se richiesto effettuo prima la storicizzazione
020500160129     C                   IF        §AOADOCFLS = 'S'
020600160513     C***                EVAL      ROOT = '/QNTC/cednetappvf14/Docflow/' +
020700160513     C***                                 'ArchivioSpool/'
020800160513     C                   EVAL      ROOT = '/QNTC/cedfsbo2/Docflow/' +
020900160513     C                                    'ArchivioSpool/'
021000160120     C                   CALL(e)   'TRBMAF05C'
021100160120     C                   PARM                    IFSNAM
021200160120     C                   PARM                    FILNAM
021300160120     C                   PARM                    JOBNAM
021400160120     C                   PARM                    USRNAM
021500160120     C                   PARM                    JOBNUM
021600160120     C                   PARM                    NUMFIL
021700160120     C                   PARM                    ROOT
021800160120     C                   PARM                    SUBFLDR
021900160120     C                   PARM                    TRBMFCDS
022000160120     C                   PARM                    FILEOUT
022100160120     C                   PARM      wDATAORA      DATAORA          15
022200160120     C                   PARM                    FLGSUBFLDR
022300171011     C                   PARM      'S'           wFLGREPLACE
022400160120     C                   PARM                    ESITO
022500160120     C                   ENDIF
022600150618     C*
022700160129     C                   IF        §AOADOCFLW = 'P'
022800160513     C***                EVAL      ROOT = '/QNTC/cednetappvf14/Docflow/' +
022900160513     C***                                 'Corrente/'
023000160513     C                   EVAL      ROOT = '/QNTC/cedfsbo2/Docflow/' +
023100160513     C                                    'Corrente/'
023200150618     C                   CALL(e)   'TRBMAF05C'
023300150618     C                   PARM                    IFSNAM
023400150618     C                   PARM                    FILNAM
023500150618     C                   PARM                    JOBNAM
023600150618     C                   PARM                    USRNAM
023700150618     C                   PARM                    JOBNUM
023800150618     C                   PARM                    NUMFIL
023900150618     C                   PARM                    ROOT
024000150618     C                   PARM                    SUBFLDR
024100150618     C                   PARM                    TRBMFCDS
024200160112     C                   PARM                    FILEOUT
024300160112     C                   PARM      *blanks       DATAORA          15
024400160115     C                   PARM                    FLGSUBFLDR
024500160120     C                   PARM                    FLGREPLACE
024600160119     C                   PARM                    ESITO
024700150618     C                   ENDIF
024800160127     C*
024900160127     C* Se richiesto nei parametri di input => loggo richiesta corrente
025000160127     C                   IF        FLGLOG = 'S'
025100160127     C                   CLEAR                   WFMAF05000
025200160127     C                   EVAL      AF05DTEXER = %timestamp()
025300160129     C                   EVAL      AF05OPZ    = DAOA
025400160127     C                   EVAL      AF05JOBNM  = jobname
025500160127     C                   EVAL      AF05JOBUT  = jobuser
025600160127     C                   EVAL      AF05JOBNR  = jobnumber
025700160127     C                   EVAL      AF05SPLNM  = FILNAM
025800160127     C                   EVAL      AF05SPLJOB = JOBNAM
025900160127     C                   EVAL      AF05SPLUT  = USRNAM
026000160127     C                   EVAL      AF05SPLJNR = JOBNUM
026100160127     C                   EVAL      AF05SPLNR  = NUMFIL
026200160127     C                   EVAL      AF05ESITO  = ESITO
026300160127     C                   WRITE(e)  WFMAF05000
026400160127     C                   ENDIF
026500150618     C*
026600160129     C                   ELSE
026700160129     C                   EVAL      ESITO = '2'
026800160129     C                   ENDIF
026900160129     C*
027000090513     C                   SETON                                        LR
