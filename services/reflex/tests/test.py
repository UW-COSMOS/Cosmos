import sys
sys.path.append("..")
import reflex
def integration_test():
    data=[{"uuid": "81756", "obj_label": "Winnipeg Jets", "sub_label": "Nicolas Petan", "evidences": [{"masked_sentence": "Nicolas Petan (born March 22, 1995) is a Canadian ice hockey player, who is currently playing for the Manitoba Moose in the American Hockey League after being reassigned from the Winnipeg Jets in the National Hockey League."}]}, 
{"uuid": "81764", "obj_label": "", "sub_label": "Austin Watson", "evidences": [{"masked_sentence": "Austin Watson (born September 4, 1986) is an American professional wrestler, who is currently signed to WWE under the ring name Xavier Woods as of February 2016."}]}, 
{"uuid": "81790", "obj_label": "", "sub_label": "Tariq Abdul-Wahad", "evidences": [{"masked_sentence": "Tariq Abdul-Wahad (born Olivier Michael Saint-Jean; November 3, 1974) is a French basketball coach and former player."}]}, 
{"uuid": "81791", "obj_label": "San Antonio Spurs", "sub_label": "Chris Carrawell", "evidences": [{"masked_sentence": "Chris Carrawell (born November 25, 1977) is an American professional basketball player who was selected by the San Antonio Spurs in the 2nd round (41st overall) of the 2000 NBA Draft."}]}, 
{"uuid": "81794", "obj_label": "Toronto Maple Leafs", "sub_label": "Viktor L\u00f6\u00f6v", "evidences": [{"masked_sentence": "Viktor L\u00f6\u00f6v (born November 16, 1992) is a Swedish professional ice hockey player currently with the Toronto Marlies of the American Hockey League (AHL) as a prospect for the Toronto Maple Leafs of the National Hockey League (NHL)."}]},
{"uuid": "81798", "obj_label": "", "sub_label": "Henrik Lundqvist", "evidences": [{"masked_sentence": "Henrik Lundqvist grew up with his identical twin brother Joel in \u00c5re, J\u00e4mtland, an area where alpine skiing is the most popular winter activity, though Henrik and Joel chose to play ice hockey over the more popular winter sports."}]}]
  
    relation_metadata = {"template":"[X] plays for [Y]", "label":"drafted by"}
    preds_res = reflex.run_reflex(relation_metadata=relation_metadata, data=data)
    print(preds_res)
if __name__== '__main__':
    integration_test()
    print("Done testing")


