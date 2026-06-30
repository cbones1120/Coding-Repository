{
 "cells": [
  {
   "cell_type": "code",
   "execution_count": 2,
   "id": "1fb1e977-c4fb-4990-912b-34f9afdaf426",
   "metadata": {},
   "outputs": [],
   "source": [
    "import random\n",
    "import math\n",
    "from itertools import product"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 2,
   "id": "326efcd8-b2d3-492a-abc7-4f164fb4c196",
   "metadata": {},
   "outputs": [],
   "source": [
    "# ----------------------- Code Beginning for Q1 -----------------------"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 3,
   "id": "5c297584-5027-4ab5-8b78-5a03eba89792",
   "metadata": {},
   "outputs": [],
   "source": [
    "# Fixed batter outcome probabilities\n",
    "batter_probs = [\n",
    "    {\"K\": 0.6, \"1B\": 0.3, \"HR\": 0.1},  # Batter 1\n",
    "    {\"K\": 0.6, \"1B\": 0.3, \"HR\": 0.1},  # Batter 2\n",
    "    {\"K\": 0.6, \"1B\": 0.25, \"HR\": 0.15},  # Batter 3\n",
    "    \"p\",  # Batter 4 - variable\n",
    "    \"p\",  # Batter 5 - variable\n",
    "    {\"K\": 0.6, \"1B\": 0.25, \"HR\": 0.15},  # Batter 6\n",
    "    {\"K\": 0.6, \"1B\": 0.3, \"HR\": 0.1},  # Batter 7\n",
    "    {\"K\": 0.6, \"1B\": 0.3, \"HR\": 0.1},  # Batter 8\n",
    "    {\"K\": 0.6, \"1B\": 0.3, \"HR\": 0.1},  # Batter 9\n",
    "]"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 4,
   "id": "3c33c602-f564-4ca0-b21b-02b28445f9c8",
   "metadata": {},
   "outputs": [],
   "source": [
    "def get_outcome(prob_dict):\n",
    "    r = random.random()\n",
    "    if r < prob_dict[\"K\"]:\n",
    "        return \"K\"\n",
    "    elif r < prob_dict[\"K\"] + prob_dict[\"1B\"]:\n",
    "        return \"1B\"\n",
    "    else:\n",
    "        return \"HR\""
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 16,
   "id": "2c885607-d02c-4717-952b-df5e1ddb6cd1",
   "metadata": {},
   "outputs": [],
   "source": [
    "def simulate_game(p, iterations=10000000):\n",
    "    win_count = 0\n",
    "\n",
    "    # Set variable batters 4 and 5\n",
    "    batter_probs[3] = {\"K\": 0.9 - p, \"1B\": p, \"HR\": 0.1}\n",
    "    batter_probs[4] = {\"K\": 0.9 - p, \"1B\": p, \"HR\": 0.1}\n",
    "\n",
    "    for _ in range(iterations):\n",
    "        outs = 0\n",
    "        score = 0\n",
    "        runners = [0, 0]  # Bases: [1st, 3rd] no 2nd because given conditions no runners can be on second base\n",
    "        batter_index = 0\n",
    "\n",
    "        while outs < 3 and score < 3:\n",
    "            batter = batter_probs[batter_index % 9]\n",
    "            result = get_outcome(batter)\n",
    "\n",
    "            if result == \"K\":\n",
    "                outs += 1\n",
    "            elif result == \"1B\":\n",
    "                # Advance runners\n",
    "                score += runners[1]  # runner on 3rd scores\n",
    "                runners[1] = runners[0]  # 1st to 3rd\n",
    "                runners[0] = 1  # batter to 1st\n",
    "            elif result == \"HR\":\n",
    "                score += 1 + sum(runners)\n",
    "                runners = [0, 0]\n",
    "\n",
    "            batter_index += 1\n",
    "\n",
    "        if score >= 3:\n",
    "            win_count += 1\n",
    "\n",
    "    return win_count / iterations"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 17,
   "id": "67116776-67d3-470d-9198-d09a0c336878",
   "metadata": {},
   "outputs": [],
   "source": [
    "# Function to find p that will lead to a 20% chance of a win. Iterates over values of p until it finds correct p.\n",
    "# Note high starts at 0.9 not 1 because of the 10% chance of a homerun.\n",
    "def find_p(target_prob=0.20, tol=0.001):\n",
    "    low = 0.0\n",
    "    high = 0.9\n",
    "    mid = (low + high) / 2\n",
    "\n",
    "    while high - low > tol:\n",
    "        prob = simulate_game(mid)\n",
    "        if prob < target_prob:\n",
    "            low = mid\n",
    "        else:\n",
    "            high = mid\n",
    "        mid = (low + high) / 2\n",
    "\n",
    "    return mid"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 18,
   "id": "c10d4484-bf65-49e3-b1b1-47e506897918",
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "Estimated value of p for 20% win probability: 0.3784\n"
     ]
    }
   ],
   "source": [
    "# Run the binary search to estimate p\n",
    "estimated_p = find_p()\n",
    "print(f\"Estimated value of p for 20% win probability: {estimated_p:.4f}\")"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 8,
   "id": "75256293-de0b-4ea1-9101-5721f58aefd8",
   "metadata": {},
   "outputs": [],
   "source": [
    "# ----------------------- Code Beginning for Q2 -----------------------"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 9,
   "id": "bf5a25cd-6382-4da4-995e-c7ed082efef3",
   "metadata": {},
   "outputs": [],
   "source": [
    "def choose(n, k):\n",
    "    return math.comb(n, k)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 10,
   "id": "b2456b70-5596-4a95-8fb5-a11dd4bcff71",
   "metadata": {},
   "outputs": [],
   "source": [
    "def binom_probs(n):\n",
    "    return [choose(n, k) / 2**n for k in range(n+1)]"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 11,
   "id": "93d3a299-6f42-4b86-9a82-e4dbf2060d60",
   "metadata": {},
   "outputs": [],
   "source": [
    "# Best-of-3 series probability\n",
    "p_game = 0.55\n",
    "p_series_better = choose(3,2)*(p_game**2)*(1-p_game) + p_game**3 #0.57475\n",
    "p_series_worse  = 1 - p_series_better #0.42525\n",
    "\n",
    "# Teams are ordered from Sept. 23 rankings.\n",
    "teams = [\"Brewers\",\"Phillies\",\"Dodgers\",\"Cubs\",\"Padres\",\"Mets\",\"Reds\",\"Dbacks\"]\n",
    "tb = {team:i for i,team in enumerate(teams)}  # lower index wins tiebreak\n",
    "\n",
    "base_w = {\n",
    "    \"Brewers\":95, \"Phillies\":92, \"Dodgers\":88, \"Cubs\":88,\n",
    "    \"Padres\":86, \"Mets\":80, \"Reds\":80, \"Dbacks\":79\n",
    "}\n",
    "\n",
    "p2 = binom_probs(2)   # MIL–SDP\n",
    "p3 = binom_probs(3)   # all 3-game blocks\n",
    "p6 = binom_probs(6)   # PHI, CIN"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 12,
   "id": "1f0cfa50-929a-4943-ae1a-d1adb46a4ca0",
   "metadata": {},
   "outputs": [],
   "source": [
    "# Takes in w which is a python dictionary that maps team to win total and picks highest win total for each division.\n",
    "# candidates is a list that will include a subsection of all teams in consideration for the playoffs.\n",
    "# Ties a broken by rank in tb, meaning higher rank team from Sept 23 breaks tie.\n",
    "def best_team(w, candidates):\n",
    "    mx = max(w[t] for t in candidates)\n",
    "    tied = [t for t in candidates if w[t] == mx]\n",
    "    return min(tied, key=lambda t: tb[t])"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 13,
   "id": "e750c290-1fd4-42e3-ba90-038a53ad0e9b",
   "metadata": {},
   "outputs": [],
   "source": [
    "# Takes in w which is a python dictionary that maps team to win total.\n",
    "# Gives seeds based on division winners finishing 1-3 and 4-6 being the remaining teams with highest win totals.\n",
    "# Again ties are broken by tb order.\n",
    "def seed_from_wins(w):\n",
    "    central = best_team(w, [\"Brewers\",\"Cubs\",\"Reds\"])\n",
    "    east    = best_team(w, [\"Phillies\",\"Mets\"])\n",
    "    west    = best_team(w, [\"Dodgers\",\"Padres\",\"Dbacks\"])\n",
    "\n",
    "    divs = [central, east, west]\n",
    "    divs.sort(key=lambda t: (-w[t], tb[t]))\n",
    "\n",
    "    s1, s2, s3 = divs\n",
    "\n",
    "    nondiv = [t for t in teams if t not in divs]\n",
    "    nondiv.sort(key=lambda t: (-w[t], tb[t]))\n",
    "\n",
    "    s4, s5, s6 = nondiv[:3]\n",
    "    return [s1,s2,s3,s4,s5,s6]"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 14,
   "id": "b0a83e93-36f3-4ce5-883a-d150871190f0",
   "metadata": {},
   "outputs": [],
   "source": [
    "# Takes in w which is a python dictionary that maps team to win total.\n",
    "# This function checks if Brewers are 1 or 2 seed and checks if Padres are an appropriate seed to potentially face them.\n",
    "# If not it returns 0, otherwise returns their probability of winning given lower or higher seed.\n",
    "def pads_ds_prob_given_season(w):\n",
    "    seeds = seed_from_wins(w)\n",
    "    bseed = seeds.index(\"Brewers\") + 1\n",
    "\n",
    "    s1,s2,s3,s4,s5,s6 = seeds\n",
    "\n",
    "    if bseed == 1:\n",
    "        if s4 == \"Padres\": return p_series_better\n",
    "        if s5 == \"Padres\": return p_series_worse\n",
    "        return 0.0\n",
    "\n",
    "    if bseed == 2:\n",
    "        if s3 == \"Padres\": return p_series_better\n",
    "        if s6 == \"Padres\": return p_series_worse\n",
    "        return 0.0\n",
    "\n",
    "    return 0.0"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 15,
   "id": "778623d4-4d34-4dd1-a1fe-a1db73e73835",
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "0.4034206023114507\n"
     ]
    }
   ],
   "source": [
    "total = 0.0\n",
    "# First for loop which calculates the probabilities of wins for head to head games (p_hh).\n",
    "# mil_hh is mil wins (0-2) against sd.\n",
    "# lad_hh is lad wins (0-3) against az.\n",
    "# chc_hh is chc wins (0-3) against nym.\n",
    "for mil_hh, lad_hh, chc_hh in product(range(3), range(4), range(4)):\n",
    "    p_hh = p2[mil_hh] * p3[lad_hh] * p3[chc_hh]\n",
    "    \n",
    "    # Each team has 3 other games against non listed teams with Binom(3, 0.5).\n",
    "    # Loop enumerates all possibilities (4^6).\n",
    "    # Computes probabilities by binomial pmfs and multiplying them.\n",
    "    for mil_o, sdp_o, lad_o, ari_o, chc_o, nym_o in product(range(4), repeat=6):\n",
    "        p_mid = (\n",
    "            p_hh *\n",
    "            p3[mil_o] * p3[sdp_o] * p3[lad_o] *\n",
    "            p3[ari_o] * p3[chc_o] * p3[nym_o]\n",
    "        )\n",
    "        \n",
    "        # Computes final wins for every team except phi and cin.\n",
    "        w = base_w.copy()\n",
    "        w[\"Brewers\"] += mil_hh + mil_o\n",
    "        w[\"Padres\"]  += (2 - mil_hh) + sdp_o\n",
    "        w[\"Dodgers\"] += lad_hh + lad_o\n",
    "        w[\"Dbacks\"]  += (3 - lad_hh) + ari_o\n",
    "        w[\"Cubs\"]    += chc_hh + chc_o\n",
    "        w[\"Mets\"]    += (3 - chc_hh) + nym_o\n",
    "\n",
    "        # Final loop calculates the wins for phi and cin because they have 6 games remaining Binom(6, 0.5).\n",
    "        for phi_o, red_o in product(range(7), repeat=2):\n",
    "            p_all = p_mid * p6[phi_o] * p6[red_o]\n",
    "\n",
    "            w2 = w.copy()\n",
    "            w2[\"Phillies\"] += phi_o\n",
    "            w2[\"Reds\"]     += red_o\n",
    "\n",
    "            # This finally updates win totals and gives probability of facing padres in the divisional series.\n",
    "            # Takes the probability of all outcomes and multiplies it by the probability the padres emerge as the divisional opponent for the brewers and sums them all \n",
    "            total += p_all * pads_ds_prob_given_season(w2)\n",
    "\n",
    "print(total)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "id": "0773c3cc-a8d3-4f24-9d4f-54de1459c8c4",
   "metadata": {},
   "outputs": [],
   "source": []
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python 3 (ipykernel)",
   "language": "python",
   "name": "python3"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.14.0"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 5
}
