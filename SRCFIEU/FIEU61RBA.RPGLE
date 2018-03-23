000100150513     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200150513     H BNDDIR('QC2LE')
000300060727     H DECEDIT('0,') DATEDIT(*DMY.)
000400140319     FWfDPDbai  UF   E             DISK    rename(test:WFDPDW)
000500150513     FWfDPDba0F O    E             DISK
000600150513      * ?   * Campi da tradurre con interi
000700150513     D atoi            PR            10I 0
000800150513     D                                     EXTPROC('atoi')
000900150513     D  Char                           *
001000150513     D                                     VALUE
001100150513     D                                     OPTIONS(*STRING)
001200150513      * ?   * Campi da tradurre con decimali
001300150513     D atof            PR             8F
001400150513     D                                     EXTPROC('atof')
001500150513     D  Char                           *
001600150513     D                                     VALUE
001700150513     D                                     OPTIONS(*STRING)
001800150513      * ?   *--------------------------------------------------------------*
001900150513     D TIDPCICS      E DS                  PREFIX(DBA:3)
002000060727      * ?================================================================== */
002100060726      * ?   *--------------------------------------------------------------*
002200060726      * ?   ( Descrizione Campi x scomposizione FLAT record )
002300060726      * ?   *--------------------------------------------------------------*
002400060726      *
002500060726      * ?   * Campi x decodifica * (INPUT  del Record)
002600060726     D  Dati           s           1500
002700060726     D Separa_campi    s              1
002800060726     D Decimal_Separ   s              1
002900060726      * ?   * Campi decodificati * (OUTPUT del Record)
003000060726      *       Schiere di Output / Campi di Output
003100060726     D  Alfa40         s             40
003200060726     d  Sk             s              3u 0
003300060726     D    NR_campi     s              3u 0
003400060726     D  Campi_Record   ds
003500060726     D    Dato_sk                    40    DIM(100)
003600060726     D  Campi_Lunghi   ds
003700060726     D    LunDato_sk                  3u 0 DIM(100)
003800060726     D Campi_Numerici  ds
003900060726     D    DatoNum_sk                  1    DIM(100)
004000060726     D Campi_Decimali  ds
004100060726     D    Decimal_sk                  3u 0 DIM(100)
004200150513      *
004300150611     D numeri_ammessi  C                   '0123456789.'
004400060726      * ?================================================================== */
004500060726      *   Ciclo principale
004600060726      * ?================================================================== */
004700140312
004800060726      *   Il primo è la testata già controllata dalla Check_trasm.
004900140319     c                   read      WFDPDbai
005000140319     c                   dow       not %eof(WFDPDbai)
005100081202      ** Decodifica record a campi variabili
005200140227     c                   exsr      Decod_Record
005300140409     c                   delete    WFDPDW
005400140409     c                   read      WFDPDbai
005500060726     C                   ENDdo
005600060726      **
005700060726     c                   SETON                                        LR
005800060726      * ?______________________________________________________________     */
005900060726      *?    Decodifica record a campi variabili
006000060726      * ?______________________________________________________________     */
006100140227     c     Decod_Record  Begsr
006200140227
006300140227      * ? Sposta il record a tracciato libero su un campo di lavoro       =
006400140303     c                   clear                   deb_cred          1
006500140319     c                   movel(p)  f1            dati
006600140227     c                   eval      dati = %Trim(Dati) + ';'
006700140227     c                   exsr      Split_Rec
006800140227     c                   exsr      Rec_Dettaglio
006900140227      **
007000140227     c                   Endsr
007100081202      * ?_______________________________________________________________    */
007200081202      *?      Riga da controllare e tradurre
007300081202      * ?_______________________________________________________________    */
007400140227     c     Rec_Dettaglio Begsr
007500150513      **
007600150513     c                   clear                   TIDPCICS
007700150513     c                   eval      DBAPRN     = Dato_sk(1)
007800150513     c                   eval      DBAYMO     = Dato_sk(2)
007900150513     c                   eval      DBAPROD    = Dato_sk(3)
008000150513     c                   eval      DBACRBU    = Dato_sk(4)
008100150513     c                   eval      DBADEBU    = Dato_sk(5)
008200150513     c                   eval      DBACRBUC   = Dato_sk(6)
008300150513     c                   eval      DBADEBUC   = Dato_sk(7)
008400150513     c                   eval      DBACRSAP   = Dato_sk(8)
008500150513     c                   eval      DBADESAP   = Dato_sk(9)
008600150513     c                   eval      DBACRSTE   = Dato_sk(10)
008700150513     c                   eval      DBADESTE   = Dato_sk(11)
008800150513     c                   eval      DBAPROC    = Dato_sk(12)
008900150513     c                   eval      DBAELEM    = Dato_sk(13)
009000150513     c                   eval      DBARAKE    = Dato_sk(14)
009100150513     c                   eval      DBAWGHT    = Dato_sk(15)
009200150513     c                   eval      DBACREDP   = Dato_sk(19)
009300150513     c                   eval      DBADEBDP   = Dato_sk(20)
009400150513      **
009500150513      * ? Nel 16° campo :                            (Oblligatorio)  Importo
009600150513     C                   z-add     16            sk
009700150513     c                   If        DatoNum_sk(sk) = 'N' or
009800150513     c                             LunDato_sk(sk) =  0
009900150513     c                   Else
010000150513     c                   eval      DBAQTY   = atoi(%TRIM(Dato_sk(sk)))
010100150513     c                   End
010200150513      **
010300150513      **
010400150513      * ? Nel 17° campo : Importo                    (Oblligatorio)  Importo
010500150513      **
010600150513      **    OCCORRE CAPIRE SE UN CREDITO O DEBITO
010700150513      **     IL DEBITO È PRECEDUTO DAL (-)
010800150513     C                   z-add     17            sk
010900150513      **
011000150513     c                   If        LunDato_sk(sk) >  0
011100150513     c                   movel     'C'           deb_cred
011200150513      *
011300150513     c                   if        %subst(Dato_sk(sk):1:1) = '-'
011400150513     c                   movel     'D'           deb_cred
011500150513     c                   eval      Dato_sk(sk) = %subst(Dato_sk(sk):2:39)
011600150513     c                   eval      LunDato_sk(sk) = LunDato_sk(sk) - 1
011700150513     c                   end
011800150513      **
011900150513      **  TOGLIE il PUNTO e lo sostituisce
012000150611      ******* ATTENZIONE ATOF vuole il punto
012100150611     c***************     '.':','       xlate     Dato_sk(sk)   Dato_sk(sk)
012200150513      **
012300150513      **  se ci sono altri caratteri al posto dei numeri e della virgola
012400150513     c     numeri_ammessiCHECK     Dato_sk(sk)   N                 3 0
012500150513     c                   if        %Found
012600150513     c                              and N <= LunDato_sk(sk)
012700150513      * ?-----> Errore
012800150513     c                   else
012900150513     c                   eval(H)   DBAVAL   = atof(%TRIM(Dato_sk(sk)))
013000150513     c                   if        deb_cred = 'D'
013100150513     c                   eval      DBAVAL = DBAVAL * (-1)
013200150513     c                   end
013300150513     c                   end
013400150513     c                   end
013500150513      **
013600150513      * ? Nel 18° campo :                            (Oblligatorio)  Importo
013700150513     C                   z-add     18            sk
013800150513     c                   If        DatoNum_sk(sk) = 'N' or
013900150513     c                             LunDato_sk(sk) =  0
014000150513     c                   Else
014100150513     c                   eval      DBAVAT   = atoi(%TRIM(Dato_sk(sk)))
014200150513     c                   End
014300150513      **
014400150513      **
014500150513      **
014600150513      * ? Nel 21° campo : Importo                    (Oblligatorio)  Importo
014700150513      **
014800150513      **    OCCORRE CAPIRE SE UN CREDITO O DEBITO
014900150513      **     IL DEBITO È PRECEDUTO DAL (-)
015000150513     C                   z-add     21            sk
015100150513      **
015200150513     c                   If        LunDato_sk(sk) >  0
015300150513     c                   movel     'C'           deb_cred
015400150513      *
015500150513     c                   if        %subst(Dato_sk(sk):1:1) = '-'
015600150513     c                   movel     'D'           deb_cred
015700150513     c                   eval      Dato_sk(sk) = %subst(Dato_sk(sk):2:39)
015800150513     c                   eval      LunDato_sk(sk) = LunDato_sk(sk) - 1
015900150513     c                   end
016000150513      **
016100150513      **  TOGLIE il PUNTO e lo sostituisce
016200150611      ******* ATTENZIONE ATOF vuole il punto
016300150611     c***************     '.':','       xlate     Dato_sk(sk)   Dato_sk(sk)
016400150513      **
016500150513      **  se ci sono altri caratteri al posto dei numeri e della virgola
016600150513     c     numeri_ammessiCHECK     Dato_sk(sk)   N                 3 0
016700150513     c                   if        %Found
016800150513     c                              and N <= LunDato_sk(sk)
016900150513      * ?-----> Errore
017000150513     c                   else
017100150513     c                   eval(H)   DBAAMO   = atof(%TRIM(Dato_sk(sk)))
017200150513     c                   if        deb_cred = 'D'
017300150513     c                   eval      DBAAMO = DBAAMO * (-1)
017400150513     c                   end
017500150513     c                   end
017600150513     c                   end
017700150513      **
017800150513     c                   write     WFDPDBA0
017900140319     c
018000081202     c                   Endsr
018100081202      * ?____________________________________________________________       */
018200081202      *?      Suddivide i campi della riga con carattere divisorio (;)
018300081202      * ?____________________________________________________________       */
018400081202     c     Split_Rec     Begsr
018500081202      **
018600081202     c                   clear                   Nr_campi
018700081202     c                   clear                   Campi_Record
018800081202     c                   clear                   Campi_Lunghi
018900081202     c                   clear                   Campi_Numerici
019000081202     c                   clear                   Campi_Decimali
019100081202     c                   movel     ';'           Separa_campi
019200150513     c                   movel     '.'           Decimal_Separ
019300081202      *
019400081202     c                   call      'FIEU00R'
019500081202      * ?     Input
019600081202     c                   parm                    Separa_campi
019700081202     c                   parm                    Decimal_Separ
019800081202     c                   parm                    dati
019900081202      * ?     Output  su schiere di 100 elementi
020000081202     c                   parm                    Nr_campi
020100081202     c                   parm                    Campi_Record                   * i dati
020200081202     c                   parm                    Campi_Lunghi                   * lunghezza dati
020300081202     c                   parm                    Campi_Numerici                 * se numerici
020400081202     c                   parm                    Campi_Decimali                 * quanti decimali
020500081202      **
020600081202     c                   Endsr
