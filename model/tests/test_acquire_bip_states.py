"""Check play-by-play extraction without network requests."""
import copy
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from acquire_bip_states import extract


class PostPlayExtraction(unittest.TestCase):
    def setUp(self):
        self.play = dict(atBatIndex=4, about=dict(inning=9, halfInning="bottom"),
            result=dict(eventType="single", awayScore=1, homeScore=2), count=dict(outs=1),
            matchup=dict(batter=dict(id=99), postOnFirst=dict(id=99)),
            playEvents=[dict(isPitch=True, index=2, pitchNumber=3, details=dict(isInPlay=True))],
            runners=[dict(details=dict(playIndex=2, isScoringEvent=True))])

    def test_exact_pitch_and_post_state(self):
        row = extract(12, 2022, dict(allPlays=[self.play]))[0]
        self.assertEqual((row["game_pk"], row["at_bat_number"], row["pitch_number"]), (12, 5, 3))
        self.assertEqual(row["api_on_1b_after"], 99)
        self.assertEqual(row["api_post_bat_score"], 2)
        self.assertEqual(row["api_scoring_runners"], 1)
        self.assertEqual(row["api_incomplete_final_half"], 1)

    def test_later_action_is_flagged_not_attributed(self):
        self.play["runners"].append(dict(details=dict(playIndex=3, isScoringEvent=True)))
        row = extract(12, 2022, dict(allPlays=[self.play]))[0]
        self.assertEqual(row["api_later_runner_moves"], 1)
        self.assertEqual(row["api_scoring_runners"], 1)

    def test_completed_half(self):
        self.play["count"]["outs"] = 3
        row = extract(12, 2022, dict(allPlays=[self.play]))[0]
        self.assertEqual(row["api_incomplete_final_half"], 0)

    def test_earlier_half_not_censored(self):
        first = copy.deepcopy(self.play)
        first["about"]["inning"] = 8
        first["atBatIndex"] = 0
        rows = extract(12, 2022, dict(allPlays=[first, self.play]))
        self.assertEqual([row["api_incomplete_final_half"] for row in rows], [0, 1])

    def test_ambiguous_event_rejected(self):
        self.play["playEvents"].append(copy.deepcopy(self.play["playEvents"][0]))
        with self.assertRaises(ValueError):
            extract(12, 2022, dict(allPlays=[self.play]))


if __name__ == "__main__":
    unittest.main()
