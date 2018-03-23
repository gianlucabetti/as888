000100130405     /*END
000200030908      *--------------------------------------------------------------------------------------------*
000300160707      * Passaggio dati al tabellone - Guida - PER AS UNICO                                         *
000400160707      * e                                                                                          *
000500160707      * Calcolo saldi (FNMSS10F) per letture (non più solo NON VDL)                                *
000600030908      *--------------------------------------------------------------------------------------------*
000700030908
000800000000     h DECEDIT('0,') DATEDIT(*DMY.)
000900030908
001000130405     fTNTBE01L  if   e           k disk
001100130405     fAZORG01L  if   e           k disk
001200030908
001300130409     d jj              S              3  0
001400000000     d kbecod          S                   LIKE(tbecod)                         *tntbe00f
001500000000     d kbeke1          S                   LIKE(tbeke1)
001600030908     d kbeke2          S                   LIKE(tbeke2)
001700030908     d comman          S             80    INZ('DLYJOB DLY(15)')                *comando CLP
001800030908     d lenght          S             15  5 INZ(45)                              *lunghezza comando
001900130405      *---
002000130405     d sk_Fil1         s              3    dim(999) inz                         *sch. Fil. NO tabell
002100020820      *---
002200130405     d Og148         e ds                  inz   qualified
002300130405     d Og150         e ds                  inz   qualified
002400130405      *---
002500000000     d param           DS                  INZ                                  *DS parametri
002600130405     d  parfil                 1      3  0 inz                                   - filiale
002700130405     d  parapl                 4      4                                          - tipo applicazione
002800030908
002900030908      *--------------------------------------------------------------------------------------------*
003000030908     c     keytbe        KLIST                                                  *tntbe02l
003100030908     c                   KFLD                    kbecod                          -tabella
003200030908     c                   KFLD                    kbeke1                          -chiave uno
003300030908     c                   KFLD                    kbeke2                          -chiave uno
003400030908      *
003500030908      * Imposta il fine ciclo
003600030908     c                   eval      *in98 = *off
003700030908      *
003800030908      * Fino a che non chiude il sottosistema
003900030908    1c                   DOW       NOT *in98
004000130405      *
004100160707      * Filiali - CON e SENZA tabellone
004200160705     c                   clear                   jj
004300160705     c                   clear                   sk_Fil1
004400130405     c     *loval        setll     AZORG
004500130405     c                   read      AZORG
004600130405do  2c                   DOW       Not  %eof(AZORG01L)
004700160707if  3c                   If        ORGfva = *blank
004800130405     c                   eval      Og148  = ORGde8
004900130405     c                   eval      Og150  = ORGdf0
005000130405if  4c                   If        Og148.§ogLPO <> *blanks  and
005100130405     c                             Og150.§ogTIS <> *blanks
005200130409     c                   eval      jj += 1
005300130409     c                   eval      sk_Fil1(jj) = %editc( ORGfil : 'X' )
005400130405e   4c                   EndIf
005500130405e   3c                   EndIf
005600130405     c                   read      AZORG
005700130405e   2c                   ENDDO
005800030908      *
005900130405      * Ciclo fino a filiali SENZA tabellone (sull'AS/400)
006000130409    2c                   FOR       jj = 1  By 1  To %elem(sk_Fil1)
006100130409    3c                   IF        sk_Fil1(jj) <= *zeros
006200130405     c                   leave
006300130405     c                   ENDIF
006400030908      * Chiama il programma di aggiornamento archivi
006500130410      * » passo comunque il tipo applicazione VDL,
006600130410      *   perchè altrimenti verrebbero memorizzati anche questi dati
006700130410      *   nel file FLTR800F «
006800160705      *?»>?In realtà il *pgm FIMS22R registra in FLTR800F solo i?
006900160705      *   ?dati di questo tipo applicazione (da lui ricevuto).  ?
007000160705      *   ?I record relativi ad altre letture (in FNMSA) vengono?
007100160705      *   ?comunque elaborati per aggiornare i saldi (in FNMSS).?<«?
007200130409     c                   move      sk_Fil1(jj)   parfil                         *filiale
007300160705     c                   movel     'V'           parapl                         *tipo applicaz. VDL
007400001027     c                   CALL      'FIMS22R'
007500000000     c                   PARM                    param
007600130405    2c                   ENDFOR                                                 *fine filiali
007700030908      *
007800030908      * Attende qualche secondo
007900030908     C                   CALL      'QCMDEXC'
008000030908     C                   PARM                    comman
008100030908     C                   PARM                    lenght
008200030908      *
008300030908      * Testa la chiusura del sottosistema
008400000000     c                   SHTDN                                        98
008500030908    1c                   ENDDO                                                  *fine sottosistema
008600030908      *
008700030908     c                   EVAL      *INLR = *ON
008800030908      *--------------------------------------------------------------------------------------------*
