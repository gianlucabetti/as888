000100170525     D iUBS715LIN      s              2A
000200170602     D iUBS715TID      s              1A
000300170602     D*                                         Tipo dati, se *blnk => UNI  = VPC
000400170602     D*                                                    se *blnk => DATI = EDIVAB
000500180312     D*                                         Tipo dati, se '1'   => UNI  = VPC
000600180312     D*                                                    se '1'   => DATI = EDIVAB + TMA
000700180312     D*                                                                       (terminal arrivo)
000800170523     D iUBS715ENV      s              3A
000900170523     D*                                         Ambiente:  'VUL' => VAS UPLOAD
001000170803     D*                                                    'VWS' => VAS WEB SERVICE ASCII
001100170803     D*                                                    'VWB' => VAS WEB SERVICE BASE64
001200170803     D*                                                    'VWF' => VAS WEB SERVICE FILE
001300170522     D iUBS715OPZ      s              4A
001400170522     D iUBS715KSU      s              7A
001500170522     D iUBS715UNI      s            256A
001600170522     D*                                         Dati tabella
001700170522     D iUBS715DATI     s           1024A
001800170522     D*                                         Dati
001900170523     D oUBS715ESI      s              1A
002000170525     D oUBS715MSG      s            256A
002100170803     D oUBS715SI95     s                   like(UBSI95DS)
002200170523     D oUBS715DATI     s               A   LEN(1000000)
