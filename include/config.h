#if __GLASGOW_HASKELL__ >= 780
#define USE_TH 1
#else
#undef USE_TH
#endif

#ifdef USE_TH
#define QUERYVALUETRANSACTION(Name,Response) queryValueTransaction ''Name Response
#define EC2VALUETRANSACTIONDEF(Name,NameStr,Tag,FilterKey) ec2ValueTransactionDef ''Name 'Name Tag FilterKey 
#define EC2VALUETRANSACTION(Name,Response) ec2ValueTransaction ''Name Response
#define ELBVALUETRANSACTIONDEF(Name,NameStr,Tag,FilterKey) elbValueTransactionDef ''Name 'Name Tag FilterKey 
#define ELBVALUETRANSACTION(Name,Response) elbValueTransaction ''Name Response

#else
#define QUERYVALUETRANSACTION(Name,Response) \
instance ResponseConsumer Name Value where { \
  type ResponseMetadata Value = QueryMetadata; \
  responseConsumer _v \
    = (queryResponseConsumer \
       $ (valueConsumer Response id)) }; \
instance Transaction Name Value

#define EC2VALUETRANSACTIONDEF(Name,NameStr,Tag,FilterKey) \
instance SignQuery Name where { \
  type ServiceConfiguration Name = QueryAPIConfiguration; \
  signQuery (Name arg_arQu) \
    = (ec2SignQuery \
       $ ([("Action", qArg NameStr), defVersion] \
          +++ (enumerate FilterKey arg_arQu qArg))) }; \
instance ResponseConsumer Name Value where { \
  type ResponseMetadata Value = QueryMetadata ; \
  responseConsumer _v \
    = (queryResponseConsumer \
       $ (valueConsumerOpt \
            (XMLValueOptions "item") Tag id)) } ;\
instance Transaction Name Value

#define EC2VALUETRANSACTION(Name,Response) \
instance ResponseConsumer Name Value where { \
  type ResponseMetadata Value = QueryMetadata ; \
  responseConsumer _v \
    = (queryResponseConsumer \
       $ (valueConsumer (Response) id)) } ; \
instance Transaction Name Value

#define ELBVALUETRANSACTIONDEF(Name,NameStr,Tag,FilterKey) \
instance SignQuery Name where { \
  type ServiceConfiguration Name = QueryAPIConfiguration ; \
  signQuery (Name arg_aHEZ) \
    = (elbSignQuery \
       $ ([("Action", qArg NameStr), defVersion] \
          +++ (enumerate FilterKey arg_aHEZ qArg))) }; \
instance ResponseConsumer Name Value where { \
  type ResponseMetadata Value = QueryMetadata ; \
  responseConsumer _ \
    = (queryResponseConsumer \
       $ (valueConsumerOpt \
            (XMLValueOptions "member") Tag id)) }; \
instance Transaction Name Value

#define ELBVALUETRANSACTION(Name,Response) \
instance ResponseConsumer Name Value where { \
  type ResponseMetadata Value = QueryMetadata ; \
  responseConsumer _v \
    = (queryResponseConsumer \
       $ (valueConsumerOpt \
            (XMLValueOptions "member") Response id)) }; \
instance Transaction Name Value

#endif
